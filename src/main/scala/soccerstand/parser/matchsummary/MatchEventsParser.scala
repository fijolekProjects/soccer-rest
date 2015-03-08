package soccerstand.parser.matchsummary

import soccerstand.parser.matchsummary.extractors.EventsExtractors._
import soccerstand.parser.matchsummary.model.MatchEvent
import soccerstand.parser.matchsummary.model.MatchEvent.MatchEventType.{AwayTeamEvent, HomeTeamEvent}
import soccerstand.parser.matchsummary.model.MatchEvent.{MatchEventType, MatchEvents}
import soccerstand.util.Slf4jLogging

import scala.collection.immutable.Seq
import scala.xml.{Node, Elem, NodeSeq}

object MatchEventsParser extends Slf4jLogging {
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
      events.containsElemWithWord("time-box")
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

  private def makeEventsTyped(events: NodeSeq): Seq[MatchEvent] = {
    val typedEvents = events.map {
      case YellowCardExtractor(yellowCardEvent)             => Right(yellowCardEvent)
      case SecondYellowCardExtractor(secondYellowCardEvent) => Right(secondYellowCardEvent)
      case RedCardExtractor(redCardEvent)                   => Right(redCardEvent)
      case SubstitutionExtractor(subsEvent)                 => Right(subsEvent)
      case MissedPenaltyExtractor(missedPenaltyEvent)       => Right(missedPenaltyEvent)
      case ScoredPenaltyExtractor(scoredPenaltyEvent)       => Right(scoredPenaltyEvent)
      case GoalExtractor(goalEvent)                         => Right(goalEvent)
      case OwnGoalExtractor(ownGoalEvent)                   => Right(ownGoalEvent)
      case unknownEvent                                     => Left(unknownEvent)
    }
    logErrors(typedEvents)
    typedEvents.collect { case Right(typedEvent) => typedEvent }
  }

  private def logErrors(typedEvents: Seq[Either[Node, MatchEvent]]): Unit = {
    val errors = typedEvents.collect { case Left(e) => e }
    errors.foreach { e => warn(s"unknown event found: $e")}
  }
}
