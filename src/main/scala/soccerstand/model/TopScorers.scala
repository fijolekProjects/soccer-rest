package soccerstand.model

import scala.xml.Node

case class TopScorers(league: League, scorers: Seq[PlayerScores])
case class PlayerScores(rank: Int, nationality: String, player: String, team: Team, goals: Int, position: Option[PlayerPosition])

object PlayerScores {
  def fromHtmlPlayerRow(playerRow: Node, league: League): PlayerScores = {
    val nationality = playerRow \@ "data-virtual-nationality"
    val position = PlayerPosition.fromString(playerRow \@ "data-virtual-position")
    val playerTd = (playerRow \ "td").map(_.text)
    val rank = playerTd(0).init.toInt
    val player = playerTd(1)
    val teamName = playerTd(2)
    val goals = playerTd(3).toInt
    PlayerScores(rank, nationality, player, Team(teamName, league), goals, position)
  }
}
sealed trait PlayerPosition
object PlayerPosition {
  case object Defender extends PlayerPosition
  case object Midfielder extends PlayerPosition
  case object Forward extends PlayerPosition

  def fromString(s: String): Option[PlayerPosition] = s match {
    case "Defender" => Some(Defender)
    case "Midfielder" => Some(Midfielder)
    case "Forward" => Some(Forward)
    case _ => None
  }
}