package soccerstand.parser

import java.util.Date

import soccerstand.indexes.{MatchFromIdIndexes, FinishedMatchIndexes, MatchIndexes}
import soccerstand.model.MatchStatus.{Finished, Live, Scheduled}
import soccerstand.model._
import soccerstand.parser.token.SoccerstandTokens.matchId

import scala.util.Try

object MatchParser {
  import soccerstand.implicits.Implicits._

  def parseFinishedMatch(matchToParse: String)(implicit league: League): FinishedMatch = {
    SoccerstandDataParser.parse(matchToParse)(FinishedMatchIndexes) { scoreIndexes =>
      fromFinishedMatchIndexes(matchToParse, scoreIndexes)
    }
  }

  private def fromFinishedMatchIndexes(matchToParse: String, matchIndexes: FinishedMatchIndexes)(implicit league: League): FinishedMatch = {
    val homeTeam = TeamMatchResult.fromIndexes(matchToParse, matchIndexes.homeTeamIdx, matchIndexes.homeTeamScoreIdx)
    val awayTeam = TeamMatchResult.fromIndexes(matchToParse, matchIndexes.awayTeamIdx, matchIndexes.awayTeamScoreIdx)
    val startDate = matchToParse.readDateAt(matchIndexes.dateIdx)
    val round = Try { matchToParse.readDataAfterIdx(matchIndexes.roundIdx) }.getOrElse("")
    val matchId = readMatchId(matchToParse)
    FinishedMatch(matchId, homeTeam, awayTeam, startDate, round)
  }

  def parseFinishedMatchNoRound(matchToParse: String)(implicit league: League): FinishedMatchNoRound = {
    SoccerstandDataParser.parse(matchToParse)(FinishedMatchIndexes) { scoreIndexes =>
      fromFinishedMatchIndexesNoRound(matchToParse, scoreIndexes)
    }
  }

  private def fromFinishedMatchIndexesNoRound(matchToParse: String, matchIndexes: FinishedMatchIndexes)(implicit league: League): FinishedMatchNoRound = {
    val homeTeam = TeamMatchResult.fromIndexes(matchToParse, matchIndexes.homeTeamIdx, matchIndexes.homeTeamScoreIdx)
    val awayTeam = TeamMatchResult.fromIndexes(matchToParse, matchIndexes.awayTeamIdx, matchIndexes.awayTeamScoreIdx)
    val startDate = matchToParse.readDateAt(matchIndexes.dateIdx)
    val matchId = readMatchId(matchToParse)
    FinishedMatchNoRound(matchId, homeTeam, awayTeam, startDate)
  }

  def parseMatch(matchToParse: String)(implicit now: Date, league: League): Match = {
    SoccerstandDataParser.parse(matchToParse)(MatchIndexes) { scoreIndexes =>
      MatchParser.fromMatchIndexes(matchToParse, scoreIndexes)
    }
  }

  private def fromMatchIndexes(matchToParse: String, matchIndexes: MatchIndexes)(implicit now: Date, league: League): Match = {
    val homeTeam = TeamMatchResult.fromIndexes(matchToParse, matchIndexes.homeTeamIdx, matchIndexes.homeTeamScoreIdx)
    val awayTeam = TeamMatchResult.fromIndexes(matchToParse, matchIndexes.awayTeamIdx, matchIndexes.awayTeamScoreIdx)
    val startDate = matchToParse.readDateAt(matchIndexes.dateIdx)
    val matchStatus = MatchStatus.fromStatusCode(matchToParse.readIntAt(matchIndexes.matchStatusIdx))
    val elapsedMinutes = elapsedMinutesForMatchStatus(startDate, matchStatus)
    val matchId = readMatchId(matchToParse)
    Match(matchId, homeTeam, awayTeam, matchStatus, startDate, elapsedMinutes)
  }

  def parseMatchFromId(matchId: String, dataFromMatchId: String, matchHtmlPage: String)(implicit league: League): Match = {
    implicit val now = new Date()
    val teamNamesAndScoresPat = "title".dataInsideTagRegex
    val teamNamesAndScores = teamNamesAndScoresPat.findFirstMatchIn(matchHtmlPage).get.group(1)
    val teamNames = teamNamesAndScores.dataAfter('|')
    val (homeTeamName, awayTeamName) = teamNames.separateAt(" - ")
    SoccerstandDataParser.parse(dataFromMatchId)(MatchFromIdIndexes) { matchFromIdIndexes =>
      val homeTeam = TeamMatchResult.fromTeamScoreIdx(dataFromMatchId, homeTeamName, matchFromIdIndexes.homeTeamScoreIdx)
      val awayTeam = TeamMatchResult.fromTeamScoreIdx(dataFromMatchId, awayTeamName, matchFromIdIndexes.awayTeamScoreIdx)
      val startDate = dataFromMatchId.readDateAt(matchFromIdIndexes.dateIdx)
      val matchStatus = MatchStatus.fromStatusCode(dataFromMatchId.readIntAt(matchFromIdIndexes.matchStatusIdx))
      val elapsedMinutes = elapsedMinutesForMatchStatus(startDate, matchStatus)
      Match(matchId, homeTeam, awayTeam, matchStatus, startDate, elapsedMinutes)
    }
  }

  private def elapsedMinutesForMatchStatus(startDate: Date, matchStatus: MatchStatus)(implicit now: Date): Option[Int] = matchStatus match {
    case Scheduled => None
    case Live => Some(calculateElapsedMinutes(startDate))
    case Finished => None
  }

  private def readMatchId(matchToParse: String): String = {
    val matchIdIdx = 3
    assert(matchToParse.take(matchIdIdx) == matchId)
    matchToParse.readDataAfterIdx(matchIdIdx)
  }

  private def calculateElapsedMinutes(startDate: Date)(implicit now: Date): Int = {
    //DOIT it should work for now, but this data should be read, not calculated
    val diffInMinutes = now.diffInMinutes(startDate)
    if (diffInMinutes > 45 && diffInMinutes <= 60) 45
    else if (diffInMinutes > 45) diffInMinutes.min(diffInMinutes - 15)
    else diffInMinutes
  }

}