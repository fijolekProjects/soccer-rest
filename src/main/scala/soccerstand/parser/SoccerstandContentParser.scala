package soccerstand.parser

import java.util.Date

import db.repository.{LeagueInfoRepository, TeamInfoRepository}
import soccerstand.model._
import soccerstand.parser.matchstats.{MatchStatistics, Stats, MatchStatisticsParser}
import soccerstand.parser.matchsummary.MatchEventsParser
import soccerstand.parser.matchsummary.model.MatchEvent.MatchEvents
import soccerstand.parser.matchsummary.model.MatchSummary

import scala.xml.{NodeSeq, Node, XML}

class SoccerstandContentParser(private val leagueInfoRepository: LeagueInfoRepository,
                               private val teamInfoRepository: TeamInfoRepository) {
  import soccerstand.implicits.Implicits._
  import soccerstand.parser.token.SoccerstandTokens._

  def parseLatestLeagueResults(soccerstandData: String): LatestFinishedMatches = {
    val inputSplittedByLeague = soccerstandData.onlyUsefulData.splitOmitFirst(newLeague).toSeq
    val leagueToParse = inputSplittedByLeague.head.split(newMatch).head
    implicit val league = League.fromString(leagueToParse)
    val leagueScores = inputSplittedByLeague.flatMap { splittedByLeague =>
      val splittedByMatches = splittedByLeague.split(newMatch)
      val matchesToParse = splittedByMatches.tail
      matchesToParse.map { MatchParser.parseFinishedMatch }.toSeq
    }
    val roundOrder = leagueScores.map(_.round).distinct.zipWithIndex.toMap
    val matchesGroupedByRound = leagueScores.groupBy(_.round).toSeq.sortBy { case (roundd, _) => roundOrder(roundd) }
    LatestFinishedMatches(league, matchesGroupedByRound)
  }

  def parseLatestTeamResults(soccerstandData: String): LatestTeamFinishedMatches = {
    val teamMatchesInLeague = extractLeagueAndMatchesToParse(soccerstandData) { case (league, matchesToParse) =>
      implicit val implLeague = league
      (league, matchesToParse.map { MatchParser.parseFinishedMatchNoRound } )
    }
    val latestTeamFinishedMatches = teamMatchesInLeague
      .groupBy { case (league, _) => league }
      .mapValues { leagueWithMatches => leagueWithMatches.unzip._2.flatten }
    val matches = latestTeamFinishedMatches.toSeq.map { case (league, matchesForLeague) => TeamMatchesInLeague(league, matchesForLeague) }
    LatestTeamFinishedMatches(matches)
  }

  def parseLiveScores(soccerstandData: String): TodayScores = {
    implicit val now = new Date()
    val leagueScores = extractLeagueAndMatchesToParse(soccerstandData) { case (league, matchesToParse) =>
      implicit val implLeague = league
      val matches = matchesToParse.map { MatchParser.parseMatch }
      LeagueScores(league, matches)
    }
    TodayScores(leagueScores)
  }

  type MatchesToParse = Seq[String]
  private def extractLeagueAndMatchesToParse[A](soccerstandData: String)(f: (League, MatchesToParse) => A): Seq[A] = {
    val inputSplittedByLeague = soccerstandData.onlyUsefulData.splitOmitFirst(newLeague).toSeq
    inputSplittedByLeague.map { splittedByLeague =>
      val splittedByMatches = splittedByLeague.split(newMatch)
      val (leagueToParse, matchesToParse) = (splittedByMatches.head, splittedByMatches.tail)
      val league = League.fromString(leagueToParse)
      f(league, matchesToParse)
    }
  }

  def parseLeagueStandings(league: League, leagueHtmlData: String): LeagueStandings = {
    val tbodyPat = "tbody".dataInsideTagRegex
    val tbodyData = tbodyPat.findFirstMatchIn(leagueHtmlData.withoutNewlines).get.group(1)
    val tbodyAsXml = XML.loadString(tbodyData.withoutNbsp.wrapInDiv)
    val nodesSplittedByTeams = tbodyAsXml \ "tr"
    val standingsForTeams = nodesSplittedByTeams.map { teamDataNode =>
      val tdValuesForTeam = teamDataNode \ "td"
      val lastTeamMatches = tdValuesForTeam.getNodeFromClass("form") \\ "a"
      val (nextMatch, pastMatches) = (lastTeamMatches.head, lastTeamMatches.tail)
      val lastMatchesTyped = pastMatches.map { oneMatch => typedMatchForTeam(league, oneMatch) }
      val teamForm = TeamForm(lastMatchesTyped)
      val teamDataExtracted = tdValuesForTeam.take(8).map(_.text).toVector
      val teamIdPattern = s"'/team/$anyContent/($anyContent)/'".r
      val teamId = teamIdPattern.findFirstMatchIn(teamDataNode.toString()).get.group(1)
      (SoccerstandTeamId(teamId), TeamStanding.fromTdValues(teamDataExtracted, league, teamForm))
    }
    LeagueStandings(league, standingsForTeams.map { case (_, standing) => standing } )
  }

  private def typedMatchForTeam(league: League, oneMatch: Node): TeamMatch = {
    val oneMatchString = oneMatch.toString()
    val matchResultStatus = MatchResults.fromNode(oneMatch)
    val (homeTeamName, awayTeamName) = oneMatchString.extractBetween('(', ')').separateAt(" - ")
    val matchDate = oneMatchString.extractBetween(')', '"').toDate
    val (homeTeamGoals, awayTeamGoals) = oneMatchString.extractBetween(']', '&').separateAt(":").map(_.toInt)
    val idToken = "glib-event-"
    val matchId = oneMatchString.substring(oneMatchString.indexOf(idToken) + idToken.length).takeWhile(_ != ' ')
    val homeTeam = TeamMatchResult(Team(homeTeamName, league), Some(homeTeamGoals))
    val awayTeam = TeamMatchResult(Team(awayTeamName, league), Some(awayTeamGoals))
    TeamMatch(matchId, homeTeam, awayTeam, matchDate, matchResultStatus)
  }

  def parseTopScorers(league: League, htmlTopScorersData: String): TopScorers = {
    val topScorersXml = XML.loadString(htmlTopScorersData)
    val playerRows = topScorersXml \\ "tbody" \ "tr"
    val scorers = playerRows.map { PlayerScores.fromHtmlPlayerRow(_, league) }
    TopScorers(league, scorers)
  }

  def parseMatchSummary(matchId: String, htmlMatchSummaryData: String, dataFromMatchId: String, matchHtmlPage: String): MatchSummary = {
    val leagueInfo = leagueInfoFromMatchHtml(matchHtmlPage)
    val matchInfo = MatchParser.parseMatchFromId(matchId, dataFromMatchId, matchHtmlPage)(leagueInfo.league)
    val matchEvents = parseMatchEvents(htmlMatchSummaryData)
    MatchSummary(leagueInfo.league, matchInfo, matchEvents)
  }

  private def parseMatchEvents(htmlMatchSummaryData: String): Option[MatchEvents] = {
    val matchSummaryFromTableRegex = "table".dataInsideTagRegex
    for {
      matchSummaryFromTable <- matchSummaryFromTableRegex.findFirstIn(htmlMatchSummaryData)
      matchSummaryAsHtml = XML.loadString(matchSummaryFromTable.withoutNbsp)
    } yield MatchEventsParser.parseMatchEvents(matchSummaryAsHtml)
  }

  def parseMatchStatistics(matchId: String, matchHtmlStats: String, dataFromMatchId: String, matchHtmlPage: String): MatchStatistics = {
    val leagueInfo = leagueInfoFromMatchHtml(matchHtmlPage)
    val matchInfo = MatchParser.parseMatchFromId(matchId, dataFromMatchId, matchHtmlPage)(leagueInfo.league)
    val stats = MatchStatisticsParser.parseMatchStatistics(matchHtmlStats)
    MatchStatistics(leagueInfo.league, matchInfo, stats)
  }

  private def leagueInfoFromMatchHtml(matchHtmlPage: String): LeagueInfo = {
    val tournamentIds = TournamentIds.fromMatchHtmlPage(matchHtmlPage)
    leagueInfoRepository.findByTournamentIds(tournamentIds)
  }

}