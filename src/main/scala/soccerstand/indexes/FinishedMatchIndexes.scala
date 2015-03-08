package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

case class FinishedMatchIndexes(homeTeamIdx: Int, homeTeamScoreIdx: Option[Int], awayTeamIdx: Int, awayTeamScoreIdx: Option[Int], dateIdx: Int, roundIdx: Int)
object FinishedMatchIndexes extends ParsingIndexes {
  val zero = FinishedMatchIndexes(-1, None, -1, None, -1, -1)
  type I = FinishedMatchIndexes
  val mappings = Seq(
    (homeTeam, (idx: Int, idxs: I) => idxs.copy(homeTeamIdx = idx)),
    (awayTeam, (idx: Int, idxs: I) => idxs.copy(awayTeamIdx = idx)),
    (homeTeamScore, (idx: Int, idxs: I) => idxs.copy(homeTeamScoreIdx = Some(idx))),
    (awayTeamScore, (idx: Int, idxs: I) => idxs.copy(awayTeamScoreIdx = Some(idx))),
    (startDate, (idx: Int, idxs: I) => idxs.copy(dateIdx = idx)),
    (round, (idx: Int, idxs: I) => idxs.copy(roundIdx = idx))
  )
}
