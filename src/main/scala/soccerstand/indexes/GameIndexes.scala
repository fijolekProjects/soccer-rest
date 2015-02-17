package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

case class GameIndexes(homeClubIdx: Int, homeClubScoreIdx: Int, awayClubIdx: Int, awayClubScoreIdx: Int, gameStatusIdx: Int, dateIdx: Int)
object GameIndexes extends ParsingIndexes {
  val zero = GameIndexes(-1, -1, -1, -1, -1, -1)
  type I = GameIndexes
  val mappings = Seq(
    (homeClub, (idx: Int, idxs: I) => idxs.copy(homeClubIdx = idx)),
    (awayClub, (idx: Int, idxs: I) => idxs.copy(awayClubIdx = idx)),
    (homeClubScore, (idx: Int, idxs: I) => idxs.copy(homeClubScoreIdx = idx)),
    (awayClubScore, (idx: Int, idxs: I) => idxs.copy(awayClubScoreIdx = idx)),
    (gameStatus, (idx: Int, idxs: I) => idxs.copy(gameStatusIdx = idx)),
    (startDate, (idx: Int, idxs: I) => idxs.copy(dateIdx = idx))
  )
}
