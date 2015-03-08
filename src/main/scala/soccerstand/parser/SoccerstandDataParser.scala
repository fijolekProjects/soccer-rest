package soccerstand.parser

import soccerstand.indexes.ParsingIndexes

import scala.annotation.tailrec

object SoccerstandDataParser {
  def parse[DomainObj](data: String)(parsingIndexes: ParsingIndexes)(indexesToDomainObj: parsingIndexes.I => DomainObj): DomainObj = {
    val tokenOffset = 3
    @tailrec
    def go(dataLeft: String, indexes: parsingIndexes.I, currentIndex: Int): parsingIndexes.I = {
      if (dataLeft.isEmpty) indexes
      else {
        val nextIndex = currentIndex + 1
        val currentDataToken = dataLeft.take(tokenOffset)
        val dataTail = dataLeft.tail
        val matchingTokenFound = parsingIndexes.mappings.find { case (token, _) => token == currentDataToken }
        matchingTokenFound match {
          case Some((_, mapping)) => go(dataTail, mapping(currentIndex + tokenOffset, indexes), nextIndex)
          case None => go(dataTail, indexes, nextIndex)
        }
      }
    }
    val scoreIndexes = go(data, parsingIndexes.zero, 0)
    indexesToDomainObj(scoreIndexes)
  }
}