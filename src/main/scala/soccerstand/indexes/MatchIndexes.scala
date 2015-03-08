package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

case class MatchIndexes(homeTeamIdx: Int, homeTeamScoreIdx: Option[Int], awayTeamIdx: Int, awayTeamScoreIdx: Option[Int], matchStatusIdx: Int, dateIdx: Int)
object MatchIndexes extends ParsingIndexes {
  val zero = MatchIndexes(-1, None, -1, None, -1, -1)
  type I = MatchIndexes
  val mappings = Seq(
    (homeTeam, (idx: Int, idxs: I) => idxs.copy(homeTeamIdx = idx)),
    (awayTeam, (idx: Int, idxs: I) => idxs.copy(awayTeamIdx = idx)),
    (homeTeamScore, (idx: Int, idxs: I) => idxs.copy(homeTeamScoreIdx = Some(idx))),
    (awayTeamScore, (idx: Int, idxs: I) => idxs.copy(awayTeamScoreIdx = Some(idx))),
    (matchStatus, (idx: Int, idxs: I) => idxs.copy(matchStatusIdx = idx)),
    (startDate, (idx: Int, idxs: I) => idxs.copy(dateIdx = idx))
  )
}
