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
    val allTdTagsPattern = "<td.*".r
    val splittedByTeams = allTdTagsPattern.findAllMatchIn(leagueHtmlData).map(_.toString()).toList
    val standingsForTeams = splittedByTeams.map { teamData =>
      val tdTagPattern = "td".dataInsideTagRegex
      val teamInfo = tdTagPattern.findAllMatchIn(teamData).toList
      val teamHtmlData = XML.loadString(teamInfo.mkString.wrapInDiv)
      val teamDataExtracted = (teamHtmlData \\ "td").take(8).map(_.text).toVector
      val teamIdPattern = s"'/team/$anyContent/($anyContent)/'".r
      val teamId = teamIdPattern.findFirstMatchIn(teamData).get.group(1)
      (SoccerstandTeamId(teamId), TeamStanding.fromTdValues(teamDataExtracted, league))
    }

    LeagueStandings(league, standingsForTeams.map { case (_, standing) => standing } )
  }

  //DOIT this method is unused right now, because of the job that saves team info
  private def saveTeamInfosAsync(standingsForTeam: Map[SoccerstandTeamId, TeamStanding], league: League): Future[Unit] = {
    import ExecutionContext.Implicits.global
    val teamInfos = standingsForTeam.map { case (id, teamStanding) => TeamInfo(id, teamStanding.team, league) }
    Future { teamInfoRepository.createOrUpdateAll(teamInfos) }
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