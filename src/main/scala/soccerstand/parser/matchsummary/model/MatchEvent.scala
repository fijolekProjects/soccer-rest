package soccerstand.parser.matchsummary.model

import soccerstand.implicits.Implicits._
import soccerstand.parser.matchsummary.model.MatchEvent.MatchMinute
import soccerstand.parser.matchsummary.model.MatchEvent.MatchStage.{PenaltiesEvents, ExtraTimeEvents, SecondHalfEvents, FirstHalfEvents}

import scala.collection.immutable.Seq
import scala.xml.Node

object MatchEvent {
  sealed trait MatchEventTeam
  object MatchEventTeam {
    case object HomeTeamEvent extends MatchEventTeam
    case object AwayTeamEvent extends MatchEventTeam
  }

  sealed trait MatchStageTag
  object MatchStageTag {
    case object FirstHalf extends MatchStageTag
    case object SecondHalf extends MatchStageTag
    case object ExtraTime extends MatchStageTag
    case object Penalties extends MatchStageTag
  }

  sealed trait MatchStage {
    val teamEvents: TeamEvents
  }

  object MatchStage {
    case class FirstHalfEvents(teamEvents: TeamEvents) extends MatchStage
    case class SecondHalfEvents(teamEvents: TeamEvents) extends MatchStage
    case class ExtraTimeEvents(teamEvents: TeamEvents) extends MatchStage
    case class PenaltiesEvents(teamEvents: TeamEvents) extends MatchStage
  }

  case class TeamEvents(events: Seq[MatchEvent])
  case class MatchEvents(homeTeam: MatchStageEvents, awayTeam: MatchStageEvents)
  case class MatchStageEvents(firstHalf: FirstHalfEvents,
                              secondHalf: SecondHalfEvents,
                              extraTime: ExtraTimeEvents,
                              penalties: PenaltiesEvents)

  case class YellowCard            (player: String,    reason: Option[String],     minute: MatchMinute) extends MatchEvent
  case class SecondYellowCard      (player: String,    reason: Option[String],     minute: MatchMinute) extends MatchEvent
  case class RedCard               (player: String,    reason: Option[String],     minute: MatchMinute) extends MatchEvent
  case class Substitution          (playerIn: String,  playerOut: String,          minute: MatchMinute) extends MatchEvent
  case class Goal                  (player: String,    assistBy: Option[String],   minute: MatchMinute) extends MatchEvent
  case class OwnGoal               (player: String,                                minute: MatchMinute) extends MatchEvent
  case class MissedPenalty         (player: String,                                minute: MatchMinute) extends PenaltyMatchEvent
  case class ScoredPenalty         (player: String,                                minute: MatchMinute) extends PenaltyMatchEvent
  case class OffMatchScoredPenalty (player: String,                                order: Int) extends PenaltyMatchEvent
  case class OffMatchMissedPenalty (player: String,                                order: Int) extends PenaltyMatchEvent

  case class MatchMinute(minute: Int, extraTime: Option[Int]) {
    def prettyPrint = extraTime match {
      case Some(extra) => s"$minute+$extra"
      case None => s"$minute"
    }
  }
  object MatchMinute {
    def fromString(minute: String): MatchMinute = {
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

sealed trait MatchEvent
