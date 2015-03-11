package soccerstand.dto

import java.util.Date

import soccerstand.model._

case class MatchDto(id: String, league: League, homeTeam: TeamMatchResult, awayTeam: TeamMatchResult, status: MatchStatus, startDate: Date, elapsedMinutes: Option[Int])

object MatchDto {
  def fromTodayScores(soccerstandContent: TodayScores): Seq[MatchDto] = {
    soccerstandContent.content.flatMap { leagueScores =>
      leagueScores.matches.map { aMatch =>
        MatchDto(aMatch.id, leagueScores.league, aMatch.homeTeam, aMatch.awayTeam, aMatch.status, aMatch.startDate, aMatch.elapsedMinutes)
      }
    }
  }
}

