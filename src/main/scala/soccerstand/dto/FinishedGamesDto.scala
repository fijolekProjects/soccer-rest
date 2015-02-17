package soccerstand.dto

import java.util.Date

import soccerstand.model.{Club, LatestFinishedGames, League}

object FinishedGamesDto {
  type Round = String
  case class LatestFinishedGamesDto(league: League, gamesWithRound: Seq[RoundGames])
  case class RoundGames(round: Round, games: Seq[FinishedGameDto])
  case class FinishedGameDto(homeClub: Club, awayClub: Club, startDate: Date)

  object LatestFinishedGamesDto {
    def toDto(finishedGames: LatestFinishedGames): LatestFinishedGamesDto = {
      val league = finishedGames.league
      val gamesWithRound = finishedGames.gamesWithRound.map {
        case (round, games) =>
          val finishedGamesDto = games.map { game => FinishedGameDto(game.homeClub, game.awayClub, game.startDate) }
          RoundGames(round, finishedGamesDto)
      }
      LatestFinishedGamesDto(league, gamesWithRound)
    }
  }
}