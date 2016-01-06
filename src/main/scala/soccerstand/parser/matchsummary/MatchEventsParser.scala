package soccerstand.parser.matchsummary

import soccerstand.parser.matchsummary.Common.MatchTeamTag
import soccerstand.parser.matchsummary.Common.MatchTeamTag.{AwayTeam, HomeTeam}
import soccerstand.parser.matchsummary.extractors.EventsExtractors._
import soccerstand.parser.matchsummary.model.MatchEvent
import soccerstand.parser.matchsummary.model.MatchEvent.MatchStage.{ExtraTimeEvents, FirstHalfEvents, PenaltiesEvents, SecondHalfEvents}
import soccerstand.parser.matchsummary.model.MatchEvent.MatchStageTag._
import soccerstand.parser.matchsummary.model.MatchEvent._
import soccerstand.parser.token.SoccerstandTokens._
import soccerstand.util.Slf4jLogging

import scala.collection.immutable.Seq
import scala.xml.{Elem, Node, NodeSeq}
import scalaz.Scalaz._
import scalaz._

object MatchEventsParser extends Slf4jLogging {
  import soccerstand.implicits.Implicits._

  def parseMatchEvents(matchSummaryAsHtml: Elem): MatchEvents = {
    val typedMatchEvents = allEventsTyped(matchSummaryAsHtml)
    val homeTeamEvents = typedMatchEvents(HomeTeam)
    val awayTeamEvents = typedMatchEvents(AwayTeam)
    val homeTeamMatchEvents = createMatchStageEvents(homeTeamEvents)
    val awayTeamMatchEvents = createMatchStageEvents(awayTeamEvents)
    MatchEvents(homeTeamMatchEvents, awayTeamMatchEvents)
  }

  private def allEventsTyped(matchSummaryAsHtml: Elem): Map[MatchTeamTag, Map[MatchStageTag, Seq[MatchEvent]]] = {
    val matchEvents = allMatchEvents(matchSummaryAsHtml)
    val matchEventsByTeam = groupEventsByTeam(matchEvents)
    matchEventsByTeam.mapValues { makeEventsTyped }.withDefaultValue(Map())
  }

  private def makeEventsTyped(matchStageWithEvents: Map[MatchStageTag, NodeSeq]): Map[MatchStageTag, Seq[MatchEvent]] = {
    val typedEvents = matchStageWithEvents.map { case (stageTag, events) =>
      stageTag -> makeEventsTyped(events, stageTag)
    }
    typedEvents.withDefaultValue(Seq())
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
        case "1st Half" => (MatchStageTag.FirstHalf, i)
        case "2nd Half" => (MatchStageTag.SecondHalf, i)
        case "Extra Time" => (MatchStageTag.ExtraTime, i)
        case "Penalties" => (MatchStageTag.Penalties, i)
      }
    }.toMap
  }

  private def onlyTimedEvents(events: NodeSeq) = {
    (events \\ "td").filter { tableCell =>
      val potentiallyTimedEvents = (tableCell \\ "div").map(_ \@ "class")
      potentiallyTimedEvents.containsElemWithWord("time-box")
    }
  }

  private def groupEventsByTeam(matchEvents: Map[MatchStageTag, NodeSeq]): Map[MatchTeamTag, Map[MatchStageTag, NodeSeq]] = {
    val groupedByStage = groupEventsByStage(matchEvents)
    val sequenced = groupedByStage.sequence
    sequenced.withDefaultValue(Map())
  }

  private def groupEventsByStage(matchEvents: Map[MatchStageTag, NodeSeq]): Map[MatchStageTag, Map[MatchTeamTag, NodeSeq]] = {
    matchEvents.mapValues { events => events.groupBy { event =>
      val eventType = event \@ "class"
      val eventTypeMark = eventType.takeRight(2)

      eventTypeMark match {
        case `homeTeamMark` => HomeTeam
        case `awayTeamMark` => AwayTeam
      }
    }}
  }

  private def makeEventsTyped(events: NodeSeq, matchStageTag: MatchStageTag): Seq[MatchEvent] = {
    matchStageTag match {
      case FirstHalf | SecondHalf | ExtraTime =>
        logUnknownEvents { events.map {
            case YellowCardExtractor(yellowCardEvent)             => yellowCardEvent.right
            case SecondYellowCardExtractor(secondYellowCardEvent) => secondYellowCardEvent.right
            case RedCardExtractor(redCardEvent)                   => redCardEvent.right
            case SubstitutionExtractor(subsEvent)                 => subsEvent.right
            case MissedPenaltyExtractor(missedPenaltyEvent)       => missedPenaltyEvent.right
            case ScoredPenaltyExtractor(scoredPenaltyEvent)       => scoredPenaltyEvent.right
            case GoalExtractor(goalEvent)                         => goalEvent.right
            case OwnGoalExtractor(ownGoalEvent)                   => ownGoalEvent.right
            case unknownEvent                                     => unknownEvent.left
          }
        }
      case Penalties =>
        logUnknownEvents { events.map {
            case OffMatchMissedPenaltyExtractor(missedPenaltyEvent) => missedPenaltyEvent.right
            case OffMatchScoredPenaltyExtractor(scoredPenaltyEvent) => scoredPenaltyEvent.right
            case unknownEvent                                       => unknownEvent.left
          }
        }
    }
  }

  private def logUnknownEvents(extracted: Seq[\/[Node, MatchEvent]]): Seq[MatchEvent] = {
    val (errors, events) = extracted.unzipBoth
    logErrors(errors)
    events
  }

  private def logErrors(errors: Seq[Node]): Unit = {
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
