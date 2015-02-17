package soccerstand.parser

import soccerstand.indexes.ParsingIndexes

import scala.annotation.tailrec

object SoccerstandDataParser {
  def parse[DomainObj](data: String)(parsingIndexes: ParsingIndexes)(indexesToDomainObj: parsingIndexes.I => DomainObj): DomainObj = {
    val tokenOffset = 3
    @tailrec
    def go(gameDataLeft: String, gameIndexes: parsingIndexes.I, currentIndex: Int): parsingIndexes.I = {
      if (gameDataLeft.isEmpty) gameIndexes
      else {
        val nextIndex = currentIndex + 1
        val gameDataCurrentToken = gameDataLeft.take(tokenOffset)
        val gameDataTail = gameDataLeft.tail
        val matchingTokenFound = parsingIndexes.mappings.find { case (token, _) => token == gameDataCurrentToken }
        matchingTokenFound match {
          case Some((_, mapping)) => go(gameDataTail, mapping(currentIndex + tokenOffset, gameIndexes), nextIndex)
          case None => go(gameDataTail, gameIndexes, nextIndex)
        }
      }
    }
    val scoreIndexes = go(data, parsingIndexes.zero, 0)
    indexesToDomainObj(scoreIndexes)
  }
}