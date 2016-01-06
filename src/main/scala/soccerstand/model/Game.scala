package soccerstand.model

import java.time.LocalDateTime

import soccerstand.dto.FinishedMatchesDto.Round

case class Match(id: String, homeTeam: TeamMatchResult, awayTeam: TeamMatchResult, status: MatchStatus, startDate: LocalDateTime, elapsedMinutes: Option[Int])

case class FinishedMatch(id: String, homeTeam: TeamMatchResult, awayTeam: TeamMatchResult, startDate: LocalDateTime, round: String)
case class FinishedMatchNoRound(id: String, homeTeam: TeamMatchResult, awayTeam: TeamMatchResult, startDate: LocalDateTime)
case class LatestFinishedMatches(league: League, matchesWithRound: Seq[(Round, Seq[FinishedMatch])])
case class TeamMatchesInLeague(league: League, matches: Seq[FinishedMatchNoRound])
case class LatestTeamFinishedMatches(leagueMatches: Seq[TeamMatchesInLeague])

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
