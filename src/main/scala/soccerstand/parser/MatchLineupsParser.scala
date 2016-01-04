package soccerstand.parser

import soccerstand.model.{League, Match}
import soccerstand.parser.MatchLineupsParser.TeamMembers.{Coach, Player}
import soccerstand.parser.matchsummary.Common.MatchTeamTag
import soccerstand.parser.matchsummary.Common.MatchTeamTag.{AwayTeam, HomeTeam}
import soccerstand.parser.token.SoccerstandTokens._

import scala.xml.{Elem, Node, NodeSeq}
import scalaz.Scalaz._
import scalaz._

object MatchLineupsParser {
  sealed trait TeamMember
  object TeamMembers {
    case class Player(name: String, number: Int, nationality: String) extends TeamMember
    case class Coach(name: String, nationality: String) extends TeamMember
  }

  sealed trait MatchTeamMemberTag
  sealed trait PlayerTeamMemberTag
  object MatchTeamMemberTags {
    case object StartingLineupPlayerTag extends MatchTeamMemberTag with PlayerTeamMemberTag
    case object PlayerOnTheBenchTag extends MatchTeamMemberTag with PlayerTeamMemberTag
    case object CoachTag extends MatchTeamMemberTag
  }

  case class MatchLineups(league: League, matchInfo: Match, lineups: Lineups)
  case class Lineups(homeTeamLineup: TeamLineup, awayTeamLineup: TeamLineup)
  case class TeamLineup(startingLineup: List[Player], bench: List[Player], coach: Option[Coach])
  case class ParsedTeamMember(matchTeamTag: MatchTeamTag, member: (Player, PlayerTeamMemberTag) \/ Coach)

  import soccerstand.implicits.Implicits._

  def parseLineups(lineupsAsHtml: Elem): Lineups = {
    val allLineupsData = (lineupsAsHtml \\ "tr").toVector
    val zero = (List.empty[ParsedTeamMember], MatchTeamMemberTags.StartingLineupPlayerTag: MatchTeamMemberTag)
    val (allTeamsMembers, _) = allLineupsData.foldLeft(zero) { case ((teamMembers, previousTag), currentLineupDataLine) =>
      val (currentTag, isNodeATag) = if ((currentLineupDataLine \@ "class").isEmpty) {
        (defineNextTeamMembers(currentLineupDataLine), true)
      } else (previousTag, false)
      if (!isNodeATag) {
        (extractMembersFromNode(currentLineupDataLine, currentTag) ::: teamMembers, currentTag)
      } else (teamMembers, currentTag)
    }
    val (homeMembers, awayMembers) = allTeamsMembers.partition { parsedMember => parsedMember.matchTeamTag == MatchTeamTag.HomeTeam }
    Lineups(extractParsedMembers(homeMembers), extractParsedMembers(awayMembers))
  }

  private def defineNextTeamMembers(curr: Node): MatchTeamMemberTag = {
    curr.text.withoutWhitespacesAtFrontAndBack match {
      case "Starting Lineups" => MatchTeamMemberTags.StartingLineupPlayerTag
      case "Substitutes" => MatchTeamMemberTags.PlayerOnTheBenchTag
      case "Coaches" => MatchTeamMemberTags.CoachTag
    }
  }

  private def extractMembersFromNode(curr: Node, currentTag: MatchTeamMemberTag): List[ParsedTeamMember] = {
    val currTeamMemberTds = onlyTimedEvents(curr).lift
    val firstMemberParsed = currTeamMemberTds(0).map(parseTeamMember(currentTag, _))
    val secondMemberParsed = currTeamMemberTds(1).map(parseTeamMember(currentTag, _))
    List(firstMemberParsed, secondMemberParsed).flatten
  }

  private def onlyTimedEvents(events: NodeSeq): NodeSeq = {
    (events \\ "td").filter { tableCell =>
      val potentiallyTimedEvents = (tableCell \\ "div").map(_ \@ "class")
      potentiallyTimedEvents.containsElemWithWord("time-box")
    }
  }

  private def parseTeamMember(matchTag: MatchTeamMemberTag, currTd: Node): ParsedTeamMember = {
    val matchTeamTag = homeOrAway(currTd)
    matchTag match {
      case playerTeamMemberTag: PlayerTeamMemberTag =>
        val player = parsePlayer(currTd)
        ParsedTeamMember(matchTeamTag, (player, playerTeamMemberTag).left)
      case MatchTeamMemberTags.CoachTag =>
        val coach = parseCoach(currTd)
        ParsedTeamMember(matchTeamTag, coach.right)
    }
  }

  private def parsePlayer(node: Node): Player = {
    val teamMemberDiv = node \ "div"
    val number = teamMemberDiv.getTextFromClass("time-box")
    val name = teamMemberDiv.getTextFromClass("name")
    val nationality = node \ "span" \@ "title"
    Player(name, number.toInt, nationality)
  }

  private def parseCoach(node: Node): Coach = {
    val name = (node \ "div").getTextFromClass("name")
    val nationality = node \ "span" \@ "title"
    Coach(name, nationality)
  }

  private def homeOrAway(event: Node): MatchTeamTag = {
    val eventType = event \@ "class"
    val eventTypeMark = eventType.takeRight(2)
    eventTypeMark match {
      case `homeTeamMark` => HomeTeam
      case `awayTeamMark` => AwayTeam
    }
  }

  private def extractParsedMembers(homeMembers: List[ParsedTeamMember]): TeamLineup = {
    val startingPlayers = homeMembers.collect { case ParsedTeamMember(_, -\/((player, MatchTeamMemberTags.StartingLineupPlayerTag))) => player }
    val bench = homeMembers.collect { case ParsedTeamMember(_, -\/((player, MatchTeamMemberTags.PlayerOnTheBenchTag))) => player }
    val coach = homeMembers.collectFirst { case ParsedTeamMember(_, \/-(c)) => c }
    TeamLineup(startingPlayers, bench, coach)
  }
}