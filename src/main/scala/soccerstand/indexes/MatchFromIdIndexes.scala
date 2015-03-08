package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

case class MatchFromIdIndexes(homeTeamScoreIdx: Option[Int], awayTeamScoreIdx: Option[Int], matchStatusIdx: Int, dateIdx: Int)
object MatchFromIdIndexes extends ParsingIndexes {
  val zero = MatchFromIdIndexes(None, None, -1, -1)
  type I = MatchFromIdIndexes
  val mappings = Seq(
    (homeTeamScoreFromMatchId, (idx: Int, idxs: I) => idxs.copy(homeTeamScoreIdx = Some(idx))),
    (awayTeamScoreFromMatchId, (idx: Int, idxs: I) => idxs.copy(awayTeamScoreIdx = Some(idx))),
    (matchStatusFromMatchId, (idx: Int, idxs: I) => idxs.copy(matchStatusIdx = idx)),
    (startDateFromMatchId, (idx: Int, idxs: I) => idxs.copy(dateIdx = idx))
  )
}