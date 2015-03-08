package soccerstand.dto

import java.util.Date

import soccerstand.model.{Team, LatestFinishedMatches, League}

object FinishedMatchesDto {
  type Round = String
  case class LatestFinishedMatchesDto(league: League, matchesWithRound: Seq[RoundMatches]) {
    def latestFirst: LatestFinishedMatchesDto =  {
      val matchesLatestFirst = matchesWithRound.sortBy(matches => matches.latestMatchDate.getTime)(Ordering[Long].reverse)
      copy(matchesWithRound = matchesLatestFirst)
    }
  }
  case class RoundMatches(round: Round, matches: Seq[FinishedMatchDto]) {
    def latestMatchDate = matches.map(_.startDate).max
  }
  case class FinishedMatchDto(id: String, homeTeam: Team, awayTeam: Team, startDate: Date)

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