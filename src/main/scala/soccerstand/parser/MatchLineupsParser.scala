package soccerstand.parser

import soccerstand.model.{League, Match}
import soccerstand.parser.MatchLineupsParser.TeamMembers.{Coach, Player}
import soccerstand.parser.matchsummary.Common.MatchTeamTag
import soccerstand.parser.matchsummary.Common.MatchTeamTag.{AwayTeam, HomeTeam}
import soccerstand.parser.token.SoccerstandTokens._

import scala.xml.{Elem, Node, NodeSeq}

object MatchLineupsParser {
  sealed trait TeamMember
  object TeamMembers {
    case class Player(name: String, number: Int, nationality: String) extends TeamMember
    case class Coach(name: String, nationality: String) extends TeamMember
  }

  sealed trait MatchTeamMemberTag
  object MatchTeamMemberTags {
    case object StartingLineupPlayerTag extends MatchTeamMemberTag
    case object PlayerOnTheBenchTag extends MatchTeamMemberTag
    case object CoachTag extends MatchTeamMemberTag
  }

  case class MatchLineups(league: League, matchInfo: Match, lineups: Lineups)
  case class Lineups(homeTeamLineup: TeamLineup, awayTeamLineup: TeamLineup)
  case class TeamLineup(startingLineup: Seq[Player], bench: Seq[Player], coach: Option[Coach])

  import soccerstand.implicits.Implicits._

  def parseLineups(lineupsAsHtml: Elem): Lineups = {
    val typedMatchEvents = allTeamMembersTyped(lineupsAsHtml)
    val homeTeamEvents = typedMatchEvents(MatchTeamTag.HomeTeam)
    val awayTeamEvents = typedMatchEvents(MatchTeamTag.AwayTeam)
    val homeTeamMatchEvents = createMatchStageEvents(homeTeamEvents)
    val awayTeamMatchEvents = createMatchStageEvents(awayTeamEvents)
    Lineups(homeTeamMatchEvents, awayTeamMatchEvents)
  }
  private def createMatchStageEvents(teamEvents: Map[MatchTeamMemberTag, Seq[TeamMember]]): TeamLineup = {
    /*fixme instanceof*/
    TeamLineup(
      startingLineup = teamEvents(MatchTeamMemberTags.StartingLineupPlayerTag).asInstanceOf[Seq[Player]],
      bench = teamEvents(MatchTeamMemberTags.PlayerOnTheBenchTag).asInstanceOf[Seq[Player]],
      coach = teamEvents(MatchTeamMemberTags.CoachTag).headOption.asInstanceOf[Option[Coach]]
    )
  }

  /*fixme deduplicate MatchEvents*/
  private def allTeamMembersTyped(matchSummaryAsHtml: Elem): Map[MatchTeamTag, Map[MatchTeamMemberTag, Seq[TeamMember]]] = {
    val matchMembers = allLineups(matchSummaryAsHtml)
    val matchMembersByTeam = groupEventsByTeam(matchMembers)
    matchMembersByTeam.mapValues { makeMembersTyped }.withDefaultValue(Map())
  }

  private def makeMembersTyped(matchStageWithMembers: Map[MatchTeamMemberTag, NodeSeq]): Map[MatchTeamMemberTag, Seq[TeamMember]] = {
    val typedMembers = matchStageWithMembers.map { case (stageTag, events) =>
      stageTag -> makeMembersTyped(events, stageTag)
    }
    typedMembers.withDefaultValue(Seq())
  }

  private def makeMembersTyped(members: NodeSeq, matchStageTag: MatchTeamMemberTag): Seq[TeamMember] = {
    matchStageTag match {
      case MatchTeamMemberTags.PlayerOnTheBenchTag | MatchTeamMemberTags.StartingLineupPlayerTag =>
        members.map { node =>
          val teamMemberDiv = node \ "div"
          val number = teamMemberDiv.getTextFromClass("time-box")
          val name = teamMemberDiv.getTextFromClass("name")
          val nationality = node \ "span" \@ "title"
          Player(name, number.toInt, nationality)
        }
      case MatchTeamMemberTags.CoachTag  =>
        members.map { node =>
          val name = (node \ "div").getTextFromClass("name")
          val nationality = node \ "span" \@ "title"
          Coach(name, nationality)
        }
    }
  }

  private def allLineups(matchLineupsAsHtml: Elem): Map[MatchTeamMemberTag, NodeSeq] = {
    val allEventsData = (matchLineupsAsHtml \\ "tr").toVector
    val allEventsDataSize = allEventsData.size
    val startEventIndexes = findMatchStageBeginningIndexes(allEventsData).withDefaultValue(allEventsDataSize)
    val startingLineupPlayersNodes = allEventsData.slice(startEventIndexes(MatchTeamMemberTags.StartingLineupPlayerTag), startEventIndexes(MatchTeamMemberTags.PlayerOnTheBenchTag))
    val playersOnTheBenchNodes = allEventsData.slice(startEventIndexes(MatchTeamMemberTags.PlayerOnTheBenchTag), startEventIndexes(MatchTeamMemberTags.CoachTag))
    val coachNodes = allEventsData.slice(startEventIndexes(MatchTeamMemberTags.CoachTag), allEventsDataSize)
    Map(
      MatchTeamMemberTags.StartingLineupPlayerTag -> onlyTimedEvents(startingLineupPlayersNodes),
      MatchTeamMemberTags.PlayerOnTheBenchTag -> onlyTimedEvents(playersOnTheBenchNodes),
      MatchTeamMemberTags.CoachTag -> onlyTimedEvents(coachNodes)
    )
  }

  private def onlyTimedEvents(events: NodeSeq) = {
    (events \\ "td").filter { tableCell =>
      val potentiallyTimedEvents = (tableCell \\ "div").map(_ \@ "class")
      potentiallyTimedEvents.containsElemWithWord("time-box")
    }
  }

  private def findMatchStageBeginningIndexes(allTeamMembersData: Vector[Node]): Map[MatchTeamMemberTag, Int] = {
    allTeamMembersData.zipWithIndex.collect { case (node, i) if (node \@ "class").isEmpty =>
      node.text.withoutWhitespacesAtFrontAndBack match {
        case "Starting Lineups" => (MatchTeamMemberTags.StartingLineupPlayerTag, i)
        case "Substitutes" => (MatchTeamMemberTags.PlayerOnTheBenchTag, i)
        case "Coaches" => (MatchTeamMemberTags.CoachTag, i)
      }
    }.toMap
  }

  private def groupEventsByTeam(matchEvents: Map[MatchTeamMemberTag, NodeSeq]): Map[MatchTeamTag, Map[MatchTeamMemberTag, NodeSeq]] = {
    val groupedByStage = groupEventsByStage(matchEvents)
    val sequenced = groupedByStage.sequence
    sequenced.withDefaultValue(Map())
  }

  private def groupEventsByStage(matchEvents: Map[MatchTeamMemberTag, NodeSeq]): Map[MatchTeamMemberTag, Map[MatchTeamTag, NodeSeq]] = {
    matchEvents.mapValues { events => events.groupBy { event =>
      val eventType = event \@ "class"
      val eventTypeMark = eventType.takeRight(2)

      eventTypeMark match {
        case `homeTeamMark` => HomeTeam
        case `awayTeamMark` => AwayTeam
      }
    }}
  }


}
