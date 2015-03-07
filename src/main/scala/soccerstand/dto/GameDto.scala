package soccerstand.dto

import java.util.Date

import soccerstand.model._

case class GameDto(id: String, league: League, homeClub: Club, awayClub: Club, status: MatchStatus, startDate: Date, elapsedMinutes: Option[Int])

object GameDto {
  def fromTodayScores(soccerstandContent: TodayScores): Seq[GameDto] = {
    soccerstandContent.content.flatMap { leagueScores =>
      leagueScores.games.map { game =>
        GameDto(game.id, leagueScores.league, game.homeClub, game.awayClub, game.status, game.startDate, game.elapsedMinutes)
      }
    }
  }
}

