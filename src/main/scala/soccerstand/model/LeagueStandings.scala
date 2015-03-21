package soccerstand.model

import db.ConvertableToDBObject

case class LeagueStandings(league: League, teams: Iterable[TeamStanding])
case class TeamInfo(id: SoccerstandTeamId, naturalId: NaturalTeamId, name: String, league: League) extends ConvertableToDBObject

case class TeamStanding(rank: Int, team: Team, matchesPlayed: Int, wins: Int, draws: Int, losses: Int, goalsScored: Int, goalsConcealed: Int, points: Int)

object TeamInfo {
  def apply(id: SoccerstandTeamId, team: Team, league: League): TeamInfo = {
    TeamInfo(id, team.naturalId, team.name, league)
  }
}

case class SoccerstandTeamId(value: String)

case class NaturalTeamId(value: String)
object NaturalTeamIdCalculator {
  import soccerstand.implicits.Implicits._
  def apply(teamName: String, league: League) = {
    val naturalLeagueId = LeagueNaturalIdCalculator(league.country.name, league.leagueName)
    NaturalTeamId(s"${teamName.withoutWhitespaces}$naturalLeagueId")
  }
}

object TeamStanding {
  def fromTdValues(values: Vector[String], league: League) = {
    val rank = values(0).init.toInt
    val teamName = values(1)
    val matchesPlayed = values(2).toInt
    val wins = values(3).toInt
    val draws = values(4).toInt
    val losses = values(5).toInt
    val goals = values(6).split(":")
    val goalsScored = goals.head.toInt
    val goalsConcealed = goals.last.toInt
    val points = values(7).toInt
    TeamStanding(rank, Team(teamName, league), matchesPlayed, wins, draws, losses, goalsScored, goalsConcealed, points)
  }
}

