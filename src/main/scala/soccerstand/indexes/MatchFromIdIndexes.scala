package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

case class MatchFromIdIndexes(homeClubScoreIdx: Option[Int], awayClubScoreIdx: Option[Int], gameStatusIdx: Int, dateIdx: Int)
object MatchFromIdIndexes extends ParsingIndexes {
  val zero = MatchFromIdIndexes(None, None, -1, -1)
  type I = MatchFromIdIndexes
  val mappings = Seq(
    (homeClubScoreFromMatchId, (idx: Int, idxs: I) => idxs.copy(homeClubScoreIdx = Some(idx))),
    (awayClubScoreFromMatchId, (idx: Int, idxs: I) => idxs.copy(awayClubScoreIdx = Some(idx))),
    (gameStatusFromMatchId, (idx: Int, idxs: I) => idxs.copy(gameStatusIdx = idx)),
    (startDateFromMatchId, (idx: Int, idxs: I) => idxs.copy(dateIdx = idx))
  )
}