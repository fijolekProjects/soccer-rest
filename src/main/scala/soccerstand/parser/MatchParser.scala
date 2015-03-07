package soccerstand.parser

import java.util.Date

import soccerstand.indexes.{MatchFromIdIndexes, FinishedMatchIndexes, MatchIndexes}
import soccerstand.model.MatchStatus.{Finished, Live, Scheduled}
import soccerstand.model._
import soccerstand.parser.token.SoccerstandTokens.gameId

import scala.util.Try

object MatchParser {
  import soccerstand.implicits.Implicits._

  def parseFinishedMatch(gameToParse: String): FinishedMatch = {
    SoccerstandDataParser.parse(gameToParse)(FinishedMatchIndexes) { scoreIndexes =>
      MatchParser.fromFinishedGameIndexes(gameToParse, scoreIndexes)
    }
  }

  private def fromFinishedGameIndexes(gameToParse: String, gameIndexes: FinishedMatchIndexes): FinishedMatch = {
    val homeClub = Club.fromIndexes(gameToParse, gameIndexes.homeClubIdx, gameIndexes.homeClubScoreIdx)
    val awayClub = Club.fromIndexes(gameToParse, gameIndexes.awayClubIdx, gameIndexes.awayClubScoreIdx)
    val startDate = gameToParse.readDateAt(gameIndexes.dateIdx)
    val round = Try { gameToParse.readDataAfterIdx(gameIndexes.roundIdx) }.getOrElse("")
    val matchId = readMatchId(gameToParse)
    FinishedMatch(matchId, homeClub, awayClub, startDate, round)
  }

  def parseMatch(matchToParse: String)(implicit now: Date): Match = {
    SoccerstandDataParser.parse(matchToParse)(MatchIndexes) { scoreIndexes =>
      MatchParser.fromGameIndexes(matchToParse, scoreIndexes)
    }
  }
  
  def parseMatchFromId(matchId: String, dataFromMatchId: String, matchHtmlPage: String): Match = {
    implicit val now = new Date()
    val teamNamesAndScoresPat = "title".dataInsideTagRegex
    val teamNamesAndScores = teamNamesAndScoresPat.findFirstMatchIn(matchHtmlPage).get.group(1)
    val teamNames = teamNamesAndScores.dataAfter('|')
    val (homeTeamName, awayTeamName) = teamNames.separateAt(" - ")
    SoccerstandDataParser.parse(dataFromMatchId)(MatchFromIdIndexes) { matchFromIdIndexes =>
      val homeClub = Club.fromClubScoreIdx(dataFromMatchId, homeTeamName, matchFromIdIndexes.homeClubScoreIdx)
      val awayClub = Club.fromClubScoreIdx(dataFromMatchId, awayTeamName, matchFromIdIndexes.awayClubScoreIdx)
      val startDate = dataFromMatchId.readDateAt(matchFromIdIndexes.dateIdx)
      val matchStatus = MatchStatus.fromStatusCode(dataFromMatchId.readIntAt(matchFromIdIndexes.gameStatusIdx))
      val elapsedMinutes = elapsedMinutesForMatchStatus(startDate, matchStatus)
      Match(matchId, homeClub, awayClub, matchStatus, startDate, elapsedMinutes)
    }
  }

  private def fromGameIndexes(gameToParse: String, gameIndexes: MatchIndexes)(implicit now: Date): Match = {
    val homeClub = Club.fromIndexes(gameToParse, gameIndexes.homeClubIdx, gameIndexes.homeClubScoreIdx)
    val awayClub = Club.fromIndexes(gameToParse, gameIndexes.awayClubIdx, gameIndexes.awayClubScoreIdx)
    val startDate = gameToParse.readDateAt(gameIndexes.dateIdx)
    val gameStatus = MatchStatus.fromStatusCode(gameToParse.readIntAt(gameIndexes.gameStatusIdx))
    val elapsedMinutes = elapsedMinutesForMatchStatus(startDate, gameStatus)
    val matchId = readMatchId(gameToParse)
    Match(matchId, homeClub, awayClub, gameStatus, startDate, elapsedMinutes)
  }

  private def elapsedMinutesForMatchStatus(startDate: Date, gameStatus: MatchStatus)(implicit now: Date): Option[Int] = gameStatus match {
    case Scheduled => None
    case Live => Some(calculateElapsedMinutes(startDate))
    case Finished => None
  }

  private def readMatchId(gameToParse: String): String = {
    val gameIdIdx = 3
    assert(gameToParse.take(gameIdIdx) == gameId)
    gameToParse.readDataAfterIdx(gameIdIdx)
  }

  private def calculateElapsedMinutes(startDate: Date)(implicit now: Date): Int = {
    //DOIT it should work for now, but this data should be read, not calculated
    val diffInMinutes = now.diffInMinutes(startDate)
    if (diffInMinutes > 45 && diffInMinutes <= 60) 45
    else if (diffInMinutes > 45) diffInMinutes.min(diffInMinutes - 15)
    else diffInMinutes
  }

}