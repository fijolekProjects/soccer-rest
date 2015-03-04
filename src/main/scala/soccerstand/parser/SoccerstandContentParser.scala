package soccerstand.parser

import java.util.Date

import db.DBFactory
import db.repository.LeagueInfoRepository
import soccerstand.model._

import scala.xml.XML

object SoccerstandContentParser {
  import soccerstand.implicits.Implicits._
  import soccerstand.parser.token.SoccerstandTokens._

  val leagueInfoRepository = new LeagueInfoRepository(DBFactory.getInstance)

  def parseLatestLeagueResults(soccerstandData: String): LatestFinishedGames = {
    val inputSplittedByLeague = soccerstandData.onlyUsefulData.splitOmitFirst(newLeague)
    val leagueToParse = inputSplittedByLeague.head.split(newGame).head
    val leagueScores = inputSplittedByLeague.flatMap { splittedByLeague =>
      val splittedByGames = splittedByLeague.split(newGame)
      val gamesToParse = splittedByGames.tail
      gamesToParse.map { GameParser.parseFinishedGame }.toSeq
    }.toSeq
    val league = League.fromString(leagueToParse)
    val roundOrder = leagueScores.map(_.round).distinct.zipWithIndex.toMap
    val gamesGroupedByRound = leagueScores.groupBy(_.round).toSeq.sortBy { case (roundd, _) => roundOrder(roundd) }
    LatestFinishedGames(league, gamesGroupedByRound)
  }
  
  def parseLiveScores(soccerstandData: String): TodayScores = {
    implicit val now = new Date()
    val inputSplittedByLeague = soccerstandData.onlyUsefulData.splitOmitFirst(newLeague)
    val leagueScores = inputSplittedByLeague.map { splittedByLeague =>
      val splittedByGames = splittedByLeague.split(newGame)
      val (leagueToParse, gamesToParse) = (splittedByGames.head, splittedByGames.tail)
      val league = League.fromString(leagueToParse)
      val games = gamesToParse.map { GameParser.parseGame }
      LeagueScores(league, games)
    }.toSeq
    TodayScores(leagueScores)
  }

  def parseLeagueStandings(league: League, leagueHtmlData: String): LeagueStandings = {
    val allTdTagsPattern = "<td.*".r
    val splittedByClubs = allTdTagsPattern.findAllMatchIn(leagueHtmlData).map(_.toString()).toList
    val standings = splittedByClubs.map { clubData =>
      val tdTagPattern = s"(?i)<td([^>]+)>($anyContent)</td>".r
      val clubInfo = tdTagPattern.findAllMatchIn(clubData).toList
      val clubHtmlData = XML.loadString(clubInfo.mkString("\n").wrapInDiv)
      val clubDataExtracted = (clubHtmlData \\ "td").take(8).map(_.text).toVector
      ClubStanding.fromTdValues(clubDataExtracted)
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

}