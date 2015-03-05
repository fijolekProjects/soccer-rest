package soccerstand.parser

import java.util.Date

import soccerstand.indexes.{FinishedGameIndexes, GameIndexes}
import soccerstand.model.GameStatus.{Finished, Scheduled, Live}
import soccerstand.model._
import soccerstand.parser.matchsummary.MatchSummaryParser
import soccerstand.parser.matchsummary.MatchSummaryParser.MatchSummary
import soccerstand.parser.token.SoccerstandTokens.gameId

import scala.util.Try
import scala.xml.XML

object GameParser {
  import soccerstand.implicits.Implicits._

  def parseFinishedGame(gameToParse: String): FinishedGame = {
    SoccerstandDataParser.parse(gameToParse)(FinishedGameIndexes) { scoreIndexes =>
      GameParser.fromFinishedGameIndexes(gameToParse, scoreIndexes)
    }
  }

  private def fromFinishedGameIndexes(gameToParse: String, gameIndexes: FinishedGameIndexes): FinishedGame = {
    val homeClub = Club.fromIndexes(gameToParse, gameIndexes.homeClubIdx, gameIndexes.homeClubScoreIdx)
    val awayClub = Club.fromIndexes(gameToParse, gameIndexes.awayClubIdx, gameIndexes.awayClubScoreIdx)
    val startDate = gameToParse.readDateAt(gameIndexes.dateIdx)
    val round = Try { gameToParse.readDataAfterIdx(gameIndexes.roundIdx) }.getOrElse("")
    val matchId = readMatchId(gameToParse)
    FinishedGame(matchId, homeClub, awayClub, startDate, round)
  }

  def parseGame(gameToParse: String)(implicit now: Date): TodayGame = {
    SoccerstandDataParser.parse(gameToParse)(GameIndexes) { scoreIndexes =>
      GameParser.fromGameIndexes(gameToParse, scoreIndexes)
    }
  }

  private def fromGameIndexes(gameToParse: String, gameIndexes: GameIndexes)(implicit now: Date): TodayGame = {
    val homeClub = Club.fromIndexes(gameToParse, gameIndexes.homeClubIdx, gameIndexes.homeClubScoreIdx)
    val awayClub = Club.fromIndexes(gameToParse, gameIndexes.awayClubIdx, gameIndexes.awayClubScoreIdx)
    val startDate = gameToParse.readDateAt(gameIndexes.dateIdx)
    val (gameStatus, elapsedMinutes) = gameToParse.readIntAt(gameIndexes.gameStatusIdx) match {
      case 1 => (Scheduled, None)
      case 2 => (Live, Some(calculateElapsedMinutes(startDate)))
      case 3 => (Finished, None)
    }
    val matchId = readMatchId(gameToParse)
    TodayGame(matchId, homeClub, awayClub, gameStatus, startDate, elapsedMinutes)
  }

  private def readMatchId(gameToParse: String): String = {
    val gameIdIdx = 3
    assert(gameToParse.take(gameIdIdx) == gameId)
    gameToParse.readDataAfterIdx(gameIdIdx)
  }

  private def calculateElapsedMinutes(startDate: Date)(implicit now: Date): Int = {
    val diffInMinutes = now.diffInMinutes(startDate)
    if (diffInMinutes > 45) diffInMinutes.min(diffInMinutes - 15) else diffInMinutes
  }

}