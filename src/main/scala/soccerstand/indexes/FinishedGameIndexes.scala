package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

case class FinishedGameIndexes(homeClubIdx: Int, homeClubScoreIdx: Int, awayClubIdx: Int, awayClubScoreIdx: Int, dateIdx: Int, roundIdx: Int)
object FinishedGameIndexes extends ParsingIndexes {
  val zero = FinishedGameIndexes(-1, -1, -1, -1, -1, -1)
  type I = FinishedGameIndexes
  val mappings = Seq(
    (homeClub, (idx: Int, idxs: I) => idxs.copy(homeClubIdx = idx)),
    (awayClub, (idx: Int, idxs: I) => idxs.copy(awayClubIdx = idx)),
    (homeClubScore, (idx: Int, idxs: I) => idxs.copy(homeClubScoreIdx = idx)),
    (awayClubScore, (idx: Int, idxs: I) => idxs.copy(awayClubScoreIdx = idx)),
    (startDate, (idx: Int, idxs: I) => idxs.copy(dateIdx = idx)),
    (round, (idx: Int, idxs: I) => idxs.copy(roundIdx = idx))
  )
}
