package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

//DOIT declare it closer to domain object - TournamentIds
case class TournamentIdsIndexes(tournamentIdIdx: Int, tournamentStageIdIdx: Int)
object TournamentIdsIndexes extends ParsingIndexes {
  override type I = TournamentIdsIndexes
  val zero = TournamentIdsIndexes(-1, -1)
  val mappings = Seq(
    (tournamentId, (idx: Int, idxs: I) => idxs.copy(tournamentIdIdx = idx)),
    (tournamentStageId, (idx: Int, idxs: I) => idxs.copy(tournamentStageIdIdx = idx))
  )
}
