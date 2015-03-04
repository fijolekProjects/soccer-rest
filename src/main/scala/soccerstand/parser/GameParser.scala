package soccerstand.parser

import java.util.Date

import soccerstand.indexes.{FinishedGameIndexes, GameIndexes}
import soccerstand.model._
import soccerstand.parser.token.SoccerstandTokens.gameId

import scala.collection.immutable.Seq
import scala.util.Try
import scala.xml.{NodeSeq, Node, XML}

object GameParser {
  import soccerstand.implicits.Implicits._

  def parseFinishedGame(gameToParse: String): FinishedGame = {
    SoccerstandDataParser.parse(gameToParse)(FinishedGameIndexes) { scoreIndexes =>
      GameParser.fromFinishedGameIndexes(gameToParse, scoreIndexes)
    }
  }

  private def fromFinishedGameIndexes(gameToParse: String, gameIndexes: FinishedGameIndexes): FinishedGame = {
    val homeClub = Club.fromIndexes(gameToParse, gameIndexes.homeClubIdx, gameIndexes.homeClubScoreIdx)
    val awayClub = Club.fromIndexes(gameToParse, gameIndexes.awayClubIdx, gameIndexes.awayClubScoreIdx)
    val startDate = gameToParse.readDateAt(gameIndexes.dateIdx)
    val round = Try { gameToParse.readDataAfterIdx(gameIndexes.roundIdx) }.getOrElse("")
    val gameIdIdx = 3
    assert(gameToParse.take(gameIdIdx) == gameId)
    val matchId = gameToParse.readDataAfterIdx(gameIdIdx)
    FinishedGame(matchId, homeClub, awayClub, startDate, round)
  }

  def parseGame(gameToParse: String)(implicit now: Date): TodayGame = {
    SoccerstandDataParser.parse(gameToParse)(GameIndexes) { scoreIndexes =>
      GameParser.fromGameIndexes(gameToParse, scoreIndexes)
    }
  }

  private def fromGameIndexes(gameToParse: String, gameIndexes: GameIndexes)(implicit now: Date): TodayGame = {
    val homeClub = Club.fromIndexes(gameToParse, gameIndexes.homeClubIdx, gameIndexes.homeClubScoreIdx)
    val awayClub = Club.fromIndexes(gameToParse, gameIndexes.awayClubIdx, gameIndexes.awayClubScoreIdx)
    val startDate = gameToParse.readDateAt(gameIndexes.dateIdx)
    val (gameStatus, elapsedMinutes) = gameToParse.readIntAt(gameIndexes.gameStatusIdx) match {
      case 1 => (Scheduled, None)
      case 2 => (Live, Some(calculateElapsedMinutes(startDate)))
      case 3 => (Finished, None)
    }
    TodayGame(homeClub, awayClub, gameStatus, startDate, elapsedMinutes)
  }
  private def calculateElapsedMinutes(startDate: Date)(implicit now: Date): Int = {
    val diffInMinutes = now.diffInMinutes(startDate)
    if (diffInMinutes > 45) diffInMinutes.min(diffInMinutes - 15) else diffInMinutes
  }
  //DOIT remove
  def main (args: Array[String]) {
    val matchSummaryHtml = scala.io.Source.fromFile("/home/mihas/projects/soccer-rest/soccerstand-site/matchSummary.txt").mkString
    parseMatchSummary(matchSummaryHtml)
  }
  
  def parseMatchSummary(matchSummaryHtml: String) = {
    val matchSummaryFromTableRegex = "table".dataInsideTagRegex
    val matchSummaryFromTable = matchSummaryFromTableRegex.findFirstIn(matchSummaryHtml).get.withoutNbsp
    val matchSummaryAsHtml = XML.loadString(matchSummaryFromTable)
    val matchEvents = (matchSummaryAsHtml \\ "tr" \\ "td").filter { tableCell => 
      val events = (tableCell \\ "div").map(_ \@ "class")
      events.contains("time-box")
    }

    val matchEventsByType = matchEvents.groupBy { event =>
      val eventType = event \@ "class"
      val eventTypeMark = eventType.takeRight(2)
      eventTypeMark match {
        case "fl" => HomeEvent
        case "fr" => AwayEvent
      }
    }
      
    val typedMatchEvents = matchEventsByType.mapValues { events => events.map {
      case YellowCard(yellowCardEvent) => yellowCardEvent
      case SecondYellowCard(secondYellowCardEvent) => secondYellowCardEvent
      case Substitution(subsEvent) => subsEvent
      case Goal(goalEvent) => goalEvent
      case _ => ()
    }
    }
    println(typedMatchEvents.mapValues(_.mkString("\n")).mkString("\n"))
    typedMatchEvents
  }
  
  
  sealed trait MatchEventType
  case object HomeEvent extends MatchEventType 
  case object AwayEvent extends MatchEventType 
  
  sealed trait MatchEvent {
    val minute: MatchMinute
  }
  
  case class MatchMinute(minute: Int) extends AnyVal
  
  case class YellowCard       (guilty: String,    reason: String,         minute: MatchMinute) extends MatchEvent
  case class SecondYellowCard (guilty: String,    reason: String,         minute: MatchMinute) extends MatchEvent
  case class RedCard          (guilty: String,    reason: String,         minute: MatchMinute) extends MatchEvent
  case class Substitution     (playerIn: String,  playerOut: String,      minute: MatchMinute) extends MatchEvent
  case class Goal             (scorer: String,    assist: Option[String], minute: MatchMinute) extends MatchEvent
  
  //DOIT: abstract over these extractors
  object YellowCard {
    def unapply(matchEvent: Node): Option[YellowCard] = {
      val spans = matchEvent \\ "span"
      if (spans.map(_ \@ "class").contains("icon y-card")) {
        val reason = spans.getTextFromClass("subincident-penalty").withoutParens
        val guilty = spans.getTextFromClass("participant-name").withoutWhitespacesAtFrontAndBack
        val minute = (matchEvent \\ "div").getTextFromClass("time-box").init
        Some(YellowCard(guilty, reason, MatchMinute(minute.toInt)))
      } else None
    }
  }
  
  object SecondYellowCard {
    def unapply(matchEvent: Node): Option[SecondYellowCard] = {
      val spans = matchEvent \\ "span"
      if (spans.map(_ \@ "class").contains("icon yr-card")) {
        val reason = spans.getTextFromClass("subincident-penalty").withoutParens
        val guilty = spans.getTextFromClass("participant-name").withoutWhitespacesAtFrontAndBack
        val minute = (matchEvent \\ "div").getTextFromClass("time-box").init
        Some(SecondYellowCard(guilty, reason, MatchMinute(minute.toInt)))
      } else None
    }
  }
  
  object Substitution {
    def unapply(matchEvent: Node): Option[Substitution] = {
      val spans = matchEvent \\ "span"
      if (spans.map(_ \@ "class").contains("icon substitution-in")) {
        val playerIn = spans.getTextFromClass("substitution-in-name").withoutWhitespacesAtFrontAndBack
        val playerOut = spans.getTextFromClass("substitution-out-name").withoutWhitespacesAtFrontAndBack
        val minute = (matchEvent \\ "div").getTextFromClass("time-box").init
        Some(Substitution(playerIn, playerOut, MatchMinute(minute.toInt)))
      } else None
    }
  }
  
  object Goal {
    def unapply(matchEvent: Node): Option[Goal] = {
      val spans = matchEvent \\ "span"
      if (spans.map(_ \@ "class").contains("icon soccer-ball")) {
        val scorer = spans.getTextFromClass("participant-name").withoutWhitespacesAtFrontAndBack
        val assist = spans.getTextFromClass("assist").withoutWhitespacesAtFrontAndBack
        val minute = (matchEvent \\ "div").getTextFromClass("time-box").init
        Some(Goal(scorer, Some(assist), MatchMinute(minute.toInt)))
      } else None
    }
  }
}