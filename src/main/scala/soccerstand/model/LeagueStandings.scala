package soccerstand.model

import db.ConvertableToDBObject

case class LeagueStandings(league: League, teams: Iterable[TeamStanding])
case class TeamStanding(rank: Int, team: Team, matchesPlayed: Int, wins: Int, draws: Int, losses: Int, goalsScored: Int, goalsConcealed: Int, points: Int)
case class TeamInfo(id: String, league: League, name: String) extends ConvertableToDBObject {
  val naturalId = NaturalTeamIdCalculator(name, league)
}

case class NaturalTeamId(value: String) extends AnyVal
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

