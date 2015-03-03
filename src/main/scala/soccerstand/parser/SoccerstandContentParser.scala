package soccerstand.parser

import java.util.Date

import db.{DBFactory, LeagueInfoRepository}
import soccerstand.model._

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
}