package soccerstand.parser.matchsummary

import soccerstand.parser.matchsummary.MatchSummaryParser.MatchEventType.{AwayTeamEvent, HomeTeamEvent}
import soccerstand.parser.matchsummary.extractors.EventsExtractors._

import scala.collection.immutable.Seq
import scala.xml.{Node, Elem, NodeSeq}

object MatchSummaryParser {
  import soccerstand.implicits.Implicits._

  def parseMatchSummary(matchSummaryAsHtml: Elem): MatchSummary = {
    val matchEvents = allMatchEvents(matchSummaryAsHtml)
    val matchEventsByType = groupEventsByType(matchEvents)
    val typedMatchEvents = matchEventsByType.mapValues { makeEventsTyped }
    MatchSummary(typedMatchEvents.getOrElse(HomeTeamEvent, Seq()), typedMatchEvents.getOrElse(AwayTeamEvent, Seq()))
  }

  private def allMatchEvents(matchSummaryAsHtml: Elem): NodeSeq = {
    (matchSummaryAsHtml \\ "tr" \\ "td").filter { tableCell =>
      val events = (tableCell \\ "div").map(_ \@ "class")
      events.containsElemWithPartOf("time-box")
    }
  }

  private def groupEventsByType(matchEvents: NodeSeq): Map[MatchEventType, NodeSeq] = {
    matchEvents.groupBy { event =>
      val eventType = event \@ "class"
      val eventTypeMark = eventType.takeRight(2)
      eventTypeMark match {
        case "fl" => HomeTeamEvent
        case "fr" => AwayTeamEvent
      }
    }
  }

  private def makeEventsTyped(events: NodeSeq): Seq[MatchEvent] = events.map {
    case YellowCardExtractor(yellowCardEvent)             => yellowCardEvent
    case SecondYellowCardExtractor(secondYellowCardEvent) => secondYellowCardEvent
    case ScoredPenaltyExtractor(scoredPenaltyEvent)       => scoredPenaltyEvent
    case GoalExtractor(goalEvent)                         => goalEvent
    case SubstitutionExtractor(subsEvent)                 => subsEvent
    case MissedPenaltyExtractor(missedPenaltyEvent)       => missedPenaltyEvent
  }

  sealed trait MatchEventType
  object MatchEventType {
    case object HomeTeamEvent extends MatchEventType
    case object AwayTeamEvent extends MatchEventType
  }

  case class MatchSummary(homeTeam: Seq[MatchEvent], awayTeam: Seq[MatchEvent])

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

  sealed trait MatchEvent {
    val minute: MatchMinute
  }
}
