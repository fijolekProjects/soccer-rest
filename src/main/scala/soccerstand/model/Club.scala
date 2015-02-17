package soccerstand.model

import soccerstand.implicits.Implicits._

case class Club(name: String, goals: Option[Int])
object Club {
  def fromIndexes(gameToParse: String, clubIdx: Int, clubScoreIdx: Int): Club = {
    val clubName = gameToParse.readDataAfterIdx(clubIdx)
    val clubScore = if (clubScoreIdx == -1) None else Some(gameToParse.readIntAt(clubScoreIdx))
    Club(clubName, clubScore)
  }
}
