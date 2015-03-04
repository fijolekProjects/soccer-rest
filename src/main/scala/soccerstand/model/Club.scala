package soccerstand.model

import soccerstand.implicits.Implicits._

case class Club(name: String, goals: Option[Int])
object Club {
  def fromIndexes(gameToParse: String, clubIdx: Int, clubScoreIdx: Option[Int]): Club = {
    val clubName = gameToParse.readDataAfterIdx(clubIdx)
    val clubScore = clubScoreIdx.map(gameToParse.readIntAt)
    Club(clubName, clubScore)
  }
}
