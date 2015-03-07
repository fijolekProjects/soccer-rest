package soccerstand.model

import java.util.Date

import soccerstand.dto.FinishedGamesDto.Round

case class Match(id: String, homeClub: Club, awayClub: Club, status: MatchStatus, startDate: Date, elapsedMinutes: Option[Int])

case class FinishedMatch(id: String, homeClub: Club, awayClub: Club, startDate: Date, round: String)
case class LatestFinishedMatches(league: League, matchesWithRound: Seq[(Round, Seq[FinishedMatch])])

sealed trait MatchStatus
object MatchStatus {
  case object Scheduled extends MatchStatus
  case object Live extends MatchStatus
  case object Finished extends MatchStatus

  def fromStatusCode(i: Int) = i match {
    case 1 => Scheduled
    case 2 => Live
    case 3 => Finished
  }
}
