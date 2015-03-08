package soccerstand.model

case class LeagueStandings(league: League, teams: Seq[TeamStanding])
case class TeamStanding(rank: Int, team: String, matchesPlayed: Int, wins: Int, draws: Int, losses: Int, goalsScored: Int, goalsConcealed: Int, points: Int)

object TeamStanding {
  def fromTdValues(values: Vector[String]) = {
    val rank = values(0).init.toInt
    val team = values(1)
    val matchesPlayed = values(2).toInt
    val wins = values(3).toInt
    val draws = values(4).toInt
    val losses = values(5).toInt
    val goals = values(6).split(":")
    val goalsScored = goals.head.toInt
    val goalsConcealed = goals.last.toInt
    val points = values(7).toInt
    TeamStanding(rank, team, matchesPlayed, wins, draws, losses, goalsScored, goalsConcealed, points)
  }
}

