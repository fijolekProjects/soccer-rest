package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

case class LeagueInfoIndexes(tournamentIdIdx: Int, tournamentStageIdIdx: Int)
object LeagueInfoIndexes extends ParsingIndexes {
  override type I = LeagueInfoIndexes
  val zero = LeagueInfoIndexes(-1, -1)
  val mappings = Seq(
    (tournamentId, (idx: Int, idxs: I) => idxs.copy(tournamentIdIdx = idx)),
    (tournamentStageId, (idx: Int, idxs: I) => idxs.copy(tournamentStageIdIdx = idx))
  )
}
