package soccerstand.indexes

import soccerstand.parser.token.SoccerstandTokens._

trait ParsingIndexes {
  type I
  type Index = Int
  val zero: I
  val mappings: Seq[(Token, (Index, I) => I)]
}
