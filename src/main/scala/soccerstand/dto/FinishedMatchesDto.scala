package soccerstand.dto

import java.time.LocalDateTime

import soccerstand.model.{LatestFinishedMatches, League, TeamMatchResult}
import soccerstand.implicits.Implicits._

object FinishedMatchesDto {
  type Round = String
  case class LatestFinishedMatchesDto(league: League, matchesWithRound: Seq[RoundMatches]) {
    def latestFirst: LatestFinishedMatchesDto =  {
      val matchesLatestFirst = matchesWithRound.sortBy(matches => matches.latestMatchDate)(Ordering[LocalDateTime].reverse)
      copy(matchesWithRound = matchesLatestFirst)
    }
  }
  case class RoundMatches(round: Round, matches: Seq[FinishedMatchDto]) {
    def latestMatchDate = matches.map(_.startDate).max
  }
  case class FinishedMatchDto(id: String, homeTeam: TeamMatchResult, awayTeam: TeamMatchResult, startDate: LocalDateTime)

  object LatestFinishedMatchesDto {
    def toDto(finishedMatches: LatestFinishedMatches): LatestFinishedMatchesDto = {
      val league = finishedMatches.league
      val matchesWithRound = finishedMatches.matchesWithRound.map {
        case (round, matches) =>
          val finishedMatchesDto = matches.map { aMatch => FinishedMatchDto(aMatch.id, aMatch.homeTeam, aMatch.awayTeam, aMatch.startDate) }
          RoundMatches(round, finishedMatchesDto)
      }
      LatestFinishedMatchesDto(league, matchesWithRound)
    }
  }
}
