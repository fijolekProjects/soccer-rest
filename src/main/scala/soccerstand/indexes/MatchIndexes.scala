package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

case class MatchIndexes(homeClubIdx: Int, homeClubScoreIdx: Option[Int], awayClubIdx: Int, awayClubScoreIdx: Option[Int], gameStatusIdx: Int, dateIdx: Int)
object MatchIndexes extends ParsingIndexes {
  val zero = MatchIndexes(-1, None, -1, None, -1, -1)
  type I = MatchIndexes
  val mappings = Seq(
    (homeClub, (idx: Int, idxs: I) => idxs.copy(homeClubIdx = idx)),
    (awayClub, (idx: Int, idxs: I) => idxs.copy(awayClubIdx = idx)),
    (homeClubScore, (idx: Int, idxs: I) => idxs.copy(homeClubScoreIdx = Some(idx))),
    (awayClubScore, (idx: Int, idxs: I) => idxs.copy(awayClubScoreIdx = Some(idx))),
    (gameStatus, (idx: Int, idxs: I) => idxs.copy(gameStatusIdx = idx)),
    (startDate, (idx: Int, idxs: I) => idxs.copy(dateIdx = idx))
  )
}
