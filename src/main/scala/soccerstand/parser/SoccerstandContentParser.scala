package soccerstand.parser

import java.util.Date

import db.repository.{TeamInfoRepository, LeagueInfoRepository}
import soccerstand.model._
import soccerstand.parser.matchsummary.MatchEventsParser
import soccerstand.parser.matchsummary.model.MatchEvent.MatchEvents
import soccerstand.parser.matchsummary.model.MatchSummary

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.XML

class SoccerstandContentParser(private val leagueInfoRepository: LeagueInfoRepository,
                               private val teamInfoRepository: TeamInfoRepository) {
  import soccerstand.implicits.Implicits._
  import soccerstand.parser.token.SoccerstandTokens._

  def parseLatestLeagueResults(soccerstandData: String): LatestFinishedMatches = {
    val inputSplittedByLeague = soccerstandData.onlyUsefulData.splitOmitFirst(newLeague)
    val leagueToParse = inputSplittedByLeague.head.split(newMatch).head
    implicit val league = League.fromString(leagueToParse)
    val leagueScores = inputSplittedByLeague.flatMap { splittedByLeague =>
      val splittedByMatches = splittedByLeague.split(newMatch)
      val matchesToParse = splittedByMatches.tail
      matchesToParse.map { MatchParser.parseFinishedMatch }.toSeq
    }.toSeq
    val roundOrder = leagueScores.map(_.round).distinct.zipWithIndex.toMap
    val matchesGroupedByRound = leagueScores.groupBy(_.round).toSeq.sortBy { case (roundd, _) => roundOrder(roundd) }
    LatestFinishedMatches(league, matchesGroupedByRound)
  }
  
  def parseLiveScores(soccerstandData: String): TodayScores = {
    implicit val now = new Date()
    val inputSplittedByLeague = soccerstandData.onlyUsefulData.splitOmitFirst(newLeague)
    val leagueScores = inputSplittedByLeague.map { splittedByLeague =>
      val splittedByMatches = splittedByLeague.split(newMatch)
      val (leagueToParse, matchesToParse) = (splittedByMatches.head, splittedByMatches.tail)
      implicit val league = League.fromString(leagueToParse)
      val matches = matchesToParse.map { MatchParser.parseMatch }
      LeagueScores(league, matches)
    }.toSeq
    TodayScores(leagueScores)
  }

  def parseLeagueStandings(league: League, leagueHtmlData: String): LeagueStandings = {
    val allTdTagsPattern = "<td.*".r
    val splittedByTeams = allTdTagsPattern.findAllMatchIn(leagueHtmlData).map(_.toString()).toList
    val standingsForTeam = splittedByTeams.map { teamData =>
      val tdTagPattern = "td".dataInsideTagRegex
      val teamInfo = tdTagPattern.findAllMatchIn(teamData).toList
      val teamHtmlData = XML.loadString(teamInfo.mkString.wrapInDiv)
      val teamDataExtracted = (teamHtmlData \\ "td").take(8).map(_.text).toVector
      val teamIdPattern = s"'/team/$anyContent/($anyContent)/'".r
      val teamId = teamIdPattern.findFirstMatchIn(teamData).get.group(1)
      (teamId, TeamStanding.fromTdValues(teamDataExtracted, league))
    }
    saveTeamInfosAsync(standingsForTeam.toMap, league)
    LeagueStandings(league, standingsForTeam.map { case (_, standing) => standing } )
  }

  private def saveTeamInfosAsync(standingsForTeam: Map[String, TeamStanding], league: League): Future[Unit] = {
    val teamInfos = standingsForTeam.map { case (id, teamStringing) => TeamInfo(id, league, teamStringing.team.name)}
    new TeamInfoSaver(teamInfoRepository).saveUnknownTeams(teamInfos)
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

  private def leagueInfoFromMatchHtml(matchHtmlPage: String): LeagueInfo = {
    val tournamentIds = TournamentIds.fromMatchHtmlPage(matchHtmlPage)
    leagueInfoRepository.findByTournamentIds(tournamentIds)
  }

  private def parseMatchEvents(htmlMatchSummaryData: String): Option[MatchEvents] = {
    val matchSummaryFromTableRegex = "table".dataInsideTagRegex
    for {
      matchSummaryFromTable <- matchSummaryFromTableRegex.findFirstIn(htmlMatchSummaryData)
      matchSummaryAsHtml = XML.loadString(matchSummaryFromTable.withoutNbsp)
    } yield MatchEventsParser.parseMatchEvents(matchSummaryAsHtml)
  }
}

class TeamInfoSaver(private val teamInfoRepository: TeamInfoRepository) {
  import ExecutionContext.Implicits.global

  def saveUnknownTeams(teams: Iterable[TeamInfo]): Future[Unit] = {
    Future { teamInfoRepository.createOrUpdateAll(teams) }
  }
}