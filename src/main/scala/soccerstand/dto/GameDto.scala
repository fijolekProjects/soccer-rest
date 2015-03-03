package soccerstand.dto

import java.util.Date

import soccerstand.model.{Club, GameStatus, League, TodayScores}

case class GameDto(league: League, homeClub: Club, awayClub: Club, status: GameStatus, startDate: Date, elapsedMinutes: Option[Int])

object GameDto {
  def fromTodayScores(soccerstandContent: TodayScores): Seq[GameDto] = {
    soccerstandContent.content.flatMap { leagueScores =>
      leagueScores.games.map { game =>
        GameDto(leagueScores.league, game.homeClub, game.awayClub, game.status, game.startDate, game.elapsedMinutes)
      }
    }
  }
}

