package soccerstand.model

import soccerstand.implicits.Implicits._

case class Team(name: String, goals: Option[Int])
object Team {
  def fromIndexes(matchToParse: String, teamIdx: Int, teamScoreIdx: Option[Int]): Team = {
    val teamName = matchToParse.readDataAfterIdx(teamIdx)
    val teamScore = teamScoreIdx.map(matchToParse.readIntAt)
    Team(teamName, teamScore)
  }
  def fromTeamScoreIdx(matchToParse: String, teamName: String, teamScoreIdx: Option[Int]): Team = {
    val teamScore = teamScoreIdx.map(matchToParse.readIntAt)
    Team(teamName, teamScore)
  }
}
