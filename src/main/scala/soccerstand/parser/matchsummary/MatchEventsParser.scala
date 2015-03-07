package soccerstand.parser.matchsummary

import soccerstand.parser.matchsummary.extractors.EventsExtractors._
import soccerstand.parser.matchsummary.model.MatchEvent
import soccerstand.parser.matchsummary.model.MatchEvent.MatchEventType.{AwayTeamEvent, HomeTeamEvent}
import soccerstand.parser.matchsummary.model.MatchEvent.{MatchEventType, MatchEvents}

import scala.collection.immutable.Seq
import scala.xml.{Elem, NodeSeq}

object MatchEventsParser {
  import soccerstand.implicits.Implicits._

  def parseMatchEvents(matchSummaryAsHtml: Elem): MatchEvents = {
    val matchEvents = allMatchEvents(matchSummaryAsHtml)
    val matchEventsByType = groupEventsByType(matchEvents)
    val typedMatchEvents = matchEventsByType.mapValues { makeEventsTyped }
    MatchEvents(typedMatchEvents.getOrElse(HomeTeamEvent, Seq()), typedMatchEvents.getOrElse(AwayTeamEvent, Seq()))
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
    case SubstitutionExtractor(subsEvent)                 => subsEvent
    case SecondYellowCardExtractor(secondYellowCardEvent) => secondYellowCardEvent
    case MissedPenaltyExtractor(missedPenaltyEvent)       => missedPenaltyEvent
    case ScoredPenaltyExtractor(scoredPenaltyEvent)       => scoredPenaltyEvent
    case GoalExtractor(goalEvent)                         => goalEvent
  }
}
