package soccerstand.parser.matchsummary

import soccerstand.parser.matchsummary.extractors.EventsExtractors._
import soccerstand.parser.matchsummary.model.MatchEvent
import soccerstand.parser.matchsummary.model.MatchEvent.MatchEventTeam.{AwayTeamEvent, HomeTeamEvent}
import soccerstand.parser.matchsummary.model.MatchEvent.MatchStage.{PenaltiesEvents, ExtraTimeEvents, SecondHalfEvents, FirstHalfEvents}
import soccerstand.parser.matchsummary.model.MatchEvent.MatchStageTag._
import soccerstand.parser.matchsummary.model.MatchEvent._
import soccerstand.util.Slf4jLogging

import scala.collection.immutable.Seq
import scala.xml.{Node, Elem, NodeSeq}

object MatchEventsParser extends Slf4jLogging {
  import soccerstand.implicits.Implicits._

  def parseMatchEvents(matchSummaryAsHtml: Elem): MatchEvents = {
    val typedMatchEvents = allEventsTyped(matchSummaryAsHtml)
    val homeTeamEvents = typedMatchEvents(HomeTeamEvent).withDefaultValue(Seq())
    val awayTeamEvents = typedMatchEvents(AwayTeamEvent).withDefaultValue(Seq())
    val homeTeamMatchEvents = createMatchStageEvents(homeTeamEvents)
    val awayTeamMatchEvents = createMatchStageEvents(awayTeamEvents)
    MatchEvents(homeTeamMatchEvents, awayTeamMatchEvents)
  }

  private def allEventsTyped(matchSummaryAsHtml: Elem): Map[MatchEventTeam, Map[MatchStageTag, Seq[MatchEvent]]] = {
    val matchEvents = allMatchEvents(matchSummaryAsHtml)
    val matchEventsByTeam = groupEventsByTeam(matchEvents)
    matchEventsByTeam.mapValues { _.mapValues(makeEventsTyped).withDefaultValue(Seq()) }.withDefaultValue(Map())
  }

  private def allMatchEvents(matchSummaryAsHtml: Elem): Map[MatchStageTag, NodeSeq] = {
    val allEventsData = (matchSummaryAsHtml \\ "tr").toVector
    val allEventsDataSize = allEventsData.size
    val startEventIndexes = findMatchStageBeginningIndexes(allEventsData).withDefaultValue(allEventsDataSize)
    val firstHalfEvents = allEventsData.slice(startEventIndexes(FirstHalf), startEventIndexes(SecondHalf))
    val secondHalfEvents = allEventsData.slice(startEventIndexes(SecondHalf), startEventIndexes(ExtraTime))
    val extraTimeEvents = allEventsData.slice(startEventIndexes(ExtraTime), startEventIndexes(Penalties))
    val penaltiesEvents = allEventsData.slice(startEventIndexes(Penalties), allEventsDataSize)
    Map(
      FirstHalf -> onlyTimedEvents(firstHalfEvents),
      SecondHalf -> onlyTimedEvents(secondHalfEvents),
      ExtraTime -> onlyTimedEvents(extraTimeEvents),
      Penalties -> onlyTimedEvents(penaltiesEvents)
    )
  }

  private def findMatchStageBeginningIndexes(allEventsData: Vector[Node]): Map[MatchStageTag, Int] = {
    allEventsData.zipWithIndex.collect { case (node, i) if (node \@ "class").contains("stage-header") =>
      node.text.withoutWhitespacesAtFrontAndBack match {
        case "1st Half" => (FirstHalf, i)
        case "2nd Half" => (SecondHalf, i)
        case "Extra Time" => (ExtraTime, i)
        case "Penalties" => (Penalties, i)
      }
    }.toMap
  }

  private def onlyTimedEvents(events: NodeSeq) = {
    (events \\ "td").filter { tableCell =>
      val events = (tableCell \\ "div").map(_ \@ "class")
      events.containsElemWithWord("time-box")
    }
  }

  private def groupEventsByTeam(matchEvents: Map[MatchStageTag, NodeSeq]): Map[MatchEventTeam, Map[MatchStageTag, NodeSeq]] = {
    val groupedByStage = groupEventsByStage(matchEvents)
    val sequenced = groupedByStage.sequence
    sequenced.withDefaultValue(Map())
  }

  private def groupEventsByStage(matchEvents: Map[MatchStageTag, NodeSeq]): Map[MatchStageTag, Map[MatchEventTeam, NodeSeq]] = {
    matchEvents.mapValues { events => events.groupBy { event =>
      val eventType = event \@ "class"
      val eventTypeMark = eventType.takeRight(2)
      eventTypeMark match {
        case "fl" => HomeTeamEvent
        case "fr" => AwayTeamEvent
      }
    }}
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

  private def createMatchStageEvents(teamEvents: Map[MatchStageTag, Seq[MatchEvent]]): MatchStageEvents = {
    MatchStageEvents(
      firstHalf = FirstHalfEvents(TeamEvents(teamEvents(FirstHalf))),
      secondHalf = SecondHalfEvents(TeamEvents(teamEvents(SecondHalf))),
      extraTime = ExtraTimeEvents(TeamEvents(teamEvents(ExtraTime))),
      penalties = PenaltiesEvents(TeamEvents(teamEvents(Penalties)))
    )
  }
}
