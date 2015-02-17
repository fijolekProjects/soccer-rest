package soccerstand.parser

import java.util.Date

import soccerstand.indexes.{FinishedGameIndexes, GameIndexes}
import soccerstand.model._

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
    val round = gameToParse.readDataAfterIdx(gameIndexes.roundIdx)
    FinishedGame(homeClub, awayClub, startDate, round)
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
    TodayGame(homeClub, awayClub, gameStatus, startDate, elapsedMinutes)
  }
  private def calculateElapsedMinutes(startDate: Date)(implicit now: Date): Int = {
    val diffInMinutes = now.diffInMinutes(startDate)
    if (diffInMinutes > 45) diffInMinutes.min(diffInMinutes - 15) else diffInMinutes
  }
}