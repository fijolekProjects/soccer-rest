package soccerstand.parser.matchsummary.model

import soccerstand.implicits.Implicits._
import soccerstand.model.Match
import soccerstand.parser.matchsummary.model.MatchEvent.MatchMinute

import scala.collection.immutable.Seq
import scala.xml.Node

object MatchEvent {
  sealed trait MatchEventType
  object MatchEventType {
    case object HomeTeamEvent extends MatchEventType
    case object AwayTeamEvent extends MatchEventType
  }

  case class MatchEvents(homeTeam: Seq[MatchEvent], awayTeam: Seq[MatchEvent])

  case class YellowCard       (player: String,    reason: String,             minute: MatchMinute) extends MatchEvent
  case class SecondYellowCard (player: String,    reason: String,             minute: MatchMinute) extends MatchEvent
  case class RedCard          (player: String,    reason: String,             minute: MatchMinute) extends MatchEvent
  case class Substitution     (playerIn: String,  playerOut: String,          minute: MatchMinute) extends MatchEvent
  case class Goal             (player: String,    assistBy: Option[String],   minute: MatchMinute) extends MatchEvent
  case class MissedPenalty    (player: String,                                minute: MatchMinute) extends PenaltyMatchEvent
  case class ScoredPenalty    (player: String,                                minute: MatchMinute) extends PenaltyMatchEvent

  case class MatchMinute(minute: Int, extraTime: Option[Int]) {
    def prettyPrint = extraTime match {
      case Some(extra) => s"$minute+$extra"
      case None => s"$minute"
    }
  }
  object MatchMinute {
    def fromMatchEvent(matchEvent: Node): MatchMinute = {
      val minute = (matchEvent \\ "div").getTextFromClass("time-box").init
      MatchMinute.fromString(minute)
    }

    private def fromString(minute: String) = {
      val minuteInExtraTime = minute.contains("+")
      if (!minuteInExtraTime) MatchMinute(minute.toInt, None)
      else {
        val (regularTime, extraTime) = minute.separateAt("+")
        MatchMinute(regularTime.toInt, Some(extraTime.toInt))
      }
    }
  }

  sealed trait PenaltyMatchEvent extends MatchEvent {
    val player: String
  }
}


sealed trait MatchEvent {
  val minute: MatchMinute
}
