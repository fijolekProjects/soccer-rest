package soccerstand.parser

import java.util.Date

import db.repository.LeagueInfoRepository
import soccerstand.model._
import soccerstand.parser.matchsummary.MatchEventsParser
import soccerstand.parser.matchsummary.model.MatchEvent.MatchEvents
import soccerstand.parser.matchsummary.model.MatchSummary

import scala.xml.XML

object SoccerstandContentParser {
  import soccerstand.implicits.Implicits._
  import soccerstand.parser.token.SoccerstandTokens._

  val leagueInfoRepository = new LeagueInfoRepository()

  def parseLatestLeagueResults(soccerstandData: String): LatestFinishedMatches = {
    val inputSplittedByLeague = soccerstandData.onlyUsefulData.splitOmitFirst(newLeague)
    val leagueToParse = inputSplittedByLeague.head.split(newMatch).head
    val leagueScores = inputSplittedByLeague.flatMap { splittedByLeague =>
      val splittedByMatches = splittedByLeague.split(newMatch)
      val matchesToParse = splittedByMatches.tail
      matchesToParse.map { MatchParser.parseFinishedMatch }.toSeq
    }.toSeq
    val league = League.fromString(leagueToParse)
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
      val league = League.fromString(leagueToParse)
      val matches = matchesToParse.map { MatchParser.parseMatch }
      LeagueScores(league, matches)
    }.toSeq
    TodayScores(leagueScores)
  }

  def parseLeagueStandings(league: League, leagueHtmlData: String): LeagueStandings = {
    val allTdTagsPattern = "<td.*".r
    val splittedByTeams = allTdTagsPattern.findAllMatchIn(leagueHtmlData).map(_.toString()).toList
    val standings = splittedByTeams.map { teamData =>
      val tdTagPattern = "td".dataInsideTagRegex
      val teamInfo = tdTagPattern.findAllMatchIn(teamData).toList
      val teamHtmlData = XML.loadString(teamInfo.mkString("\n").wrapInDiv)
      val teamDataExtracted = (teamHtmlData \\ "td").take(8).map(_.text).toVector
      TeamStanding.fromTdValues(teamDataExtracted)
    }.toList
    LeagueStandings(league, standings)
  }

  def parseTopScorers(league: League, htmlTopScorersData: String): TopScorers = {
    val topScorers = htmlTopScorersData.replaceFirst("<tfoot>(.+?)</span>", "</tbody>")
    val topScorersXml = XML.loadString(topScorers.wrapInDiv)
    val playerRows = topScorersXml \\ "tbody" \ "tr"
    val scorers = playerRows.map { PlayerScores.fromHtmlPlayerRow }
    TopScorers(league, scorers)
  }

  def parseMatchSummary(matchId: String, htmlMatchSummaryData: String, dataFromMatchId: String, matchHtmlPage: String): MatchSummary = {
    val leagueInfo = leagueInfoFromMatchHtml(matchHtmlPage)
    val matchInfo = MatchParser.parseMatchFromId(matchId, dataFromMatchId, matchHtmlPage)
    val matchEvents = parseMatchEvents(htmlMatchSummaryData)
    MatchSummary(leagueInfo.league, matchInfo, matchEvents)
  }

  private def leagueInfoFromMatchHtml(matchHtmlPage: String): LeagueInfo = {
    val tournamentIds = TournamentIds.fromMatchHtmlPage(matchHtmlPage)
    leagueInfoRepository.findByTournamentIds(tournamentIds)
  }

  private def parseMatchEvents(htmlMatchSummaryData: String): MatchEvents = {
    val matchSummaryFromTableRegex = "table".dataInsideTagRegex
    val matchSummaryFromTable = matchSummaryFromTableRegex.findFirstIn(htmlMatchSummaryData).get.withoutNbsp
    val matchSummaryAsHtml = XML.loadString(matchSummaryFromTable)
    MatchEventsParser.parseMatchEvents(matchSummaryAsHtml)
  }
}