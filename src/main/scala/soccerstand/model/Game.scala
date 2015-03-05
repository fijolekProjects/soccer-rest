package soccerstand.model

import java.util.Date

import soccerstand.dto.FinishedGamesDto.Round

case class TodayGame(id: String, homeClub: Club, awayClub: Club, status: GameStatus, startDate: Date, elapsedMinutes: Option[Int])

case class FinishedGame(id: String, homeClub: Club, awayClub: Club, startDate: Date, round: String)
case class LatestFinishedGames(league: League, gamesWithRound: Seq[(Round, Seq[FinishedGame])])

sealed trait GameStatus
object GameStatus {
  case object Scheduled extends GameStatus
  case object Live extends GameStatus
  case object Finished extends GameStatus
}
