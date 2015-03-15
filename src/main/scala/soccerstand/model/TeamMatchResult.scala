package soccerstand.model

import soccerstand.implicits.Implicits._

case class Team(naturalId: NaturalTeamId, name: String)
object Team {
  def apply(name: String, league: League): Team = {
    val naturalId = NaturalTeamIdCalculator(name, league)
    Team(naturalId, name)
  }
}
case class TeamMatchResult(team: Team, goals: Option[Int])
object TeamMatchResult {
  def fromIndexes(matchToParse: String, teamIdx: Int, teamScoreIdx: Option[Int])(implicit league: League): TeamMatchResult = {
    val teamName = matchToParse.readDataAfterIdx(teamIdx)
    val teamScore = teamScoreIdx.map(matchToParse.readIntAt)
    val naturalId = NaturalTeamIdCalculator.apply(teamName, league)
    TeamMatchResult(Team(naturalId, teamName), teamScore)
  }

  def fromTeamScoreIdx(matchToParse: String, teamName: String, teamScoreIdx: Option[Int])(implicit league: League): TeamMatchResult = {
    val teamScore = teamScoreIdx.map(matchToParse.readIntAt)
    val naturalId = NaturalTeamIdCalculator.apply(teamName, league)
    TeamMatchResult(Team(naturalId, teamName), teamScore)
  }
}
