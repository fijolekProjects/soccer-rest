package soccerstand.parser.matchsummary

import soccerstand.parser.matchsummary.MatchSummaryParser.MatchEventType.{HomeTeamEvent, AwayTeamEvent}
import soccerstand.parser.matchsummary.extractors.EventsExtractors
import EventsExtractors._

import scala.collection.immutable.Seq
import scala.xml.{Elem, NodeSeq}

object MatchSummaryParser {
  import soccerstand.implicits.Implicits._

  def parseMatchSummary(matchSummaryAsHtml: Elem): MatchSummary = {
    val matchEvents = allMatchEvents(matchSummaryAsHtml)
    val matchEventsByType = groupEventsByType(matchEvents)
    val typedMatchEvents = matchEventsByType.mapValues { makeEventsTyped }
    MatchSummary(typedMatchEvents(HomeTeamEvent), typedMatchEvents(AwayTeamEvent))
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
    case YellowCardExtractor(yellowCardEvent) => yellowCardEvent
    case SecondYellowCardExtractor(secondYellowCardEvent) => secondYellowCardEvent
    case GoalExtractor(goalEvent) => goalEvent
    case SubstitutionExtractor(subsEvent) => subsEvent
    case MissedPenaltyExtractor(missedPenaltyEvent) => missedPenaltyEvent
  }

  sealed trait MatchEventType
  object MatchEventType {
    case object HomeTeamEvent extends MatchEventType
    case object AwayTeamEvent extends MatchEventType
  }

  case class MatchSummary(homeTeam: Seq[MatchEvent], awayTeam: Seq[MatchEvent])

  case class YellowCard       (guilty: String,        reason: String,         minute: MatchMinute) extends MatchEvent
  case class SecondYellowCard (guilty: String,        reason: String,         minute: MatchMinute) extends MatchEvent
  case class RedCard          (guilty: String,        reason: String,         minute: MatchMinute) extends MatchEvent
  case class Substitution     (playerIn: String,      playerOut: String,      minute: MatchMinute) extends MatchEvent
  case class Goal             (scorer: String,        assist: Option[String], minute: MatchMinute) extends MatchEvent
  case class MissedPenalty    (unluckyFellow: String,                         minute: MatchMinute) extends MatchEvent

  case class MatchMinute(minute: Int, extraTime: Option[Int])
  object MatchMinute {
    def fromString(minute: String) = {
      val minuteInExtraTime = minute.contains("+")
      if (!minuteInExtraTime) MatchMinute(minute.toInt, None)
      else {
        val (regularTime, extraTime) = minute.separateAt("+")
        MatchMinute(regularTime.toInt, Some(extraTime.toInt))
      }
    }
  }

  sealed trait MatchEvent {
    val minute: MatchMinute
  }
}
