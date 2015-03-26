package soccerstand.model

import java.util.Date

import db.ConvertableToDBObject

import scala.xml.Node

case class LeagueStandings(league: League, teams: Iterable[TeamStanding])
case class TeamInfo(id: SoccerstandTeamId, naturalId: NaturalTeamId, name: String, league: League) extends ConvertableToDBObject

case class TeamStanding(rank: Int,
                        team: Team,
                        matchesPlayed: Int,
                        wins: Int,
                        draws: Int,
                        losses: Int,
                        goalsScored: Int,
                        goalsConcealed: Int,
                        points: Int,
                        form: TeamForm)

case class TeamForm(lastMatches: Seq[TeamMatch])
case class TeamMatch(id: String, homeTeam: TeamMatchResult, awayTeam: TeamMatchResult, startDate: Date, result: MatchResultStatus)

sealed trait MatchResultStatus
object MatchResults {
  case object Win extends MatchResultStatus
  case object Draw extends MatchResultStatus
  case object Lose extends MatchResultStatus
  
  def fromNode(n: Node): MatchResultStatus = {
    val nodeClass = n \@ "class"
    if (nodeClass.contains("form-w")) MatchResults.Win
    else if (nodeClass.contains("form-d")) MatchResults.Draw
    else if (nodeClass.contains("form-l")) MatchResults.Lose
    else throw new IllegalArgumentException("node doesnt have any of known match result")
  }
}

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
  def fromTdValues(values: Vector[String], league: League, form: TeamForm) = {
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
    TeamStanding(rank, Team(teamName, league), matchesPlayed, wins, draws, losses, goalsScored, goalsConcealed, points, form)
  }
}

