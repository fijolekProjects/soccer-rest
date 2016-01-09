package soccerstand.parser

import soccerstand.model.{League, Match}
import soccerstand.parser.matchsummary.extractors.TimeBoxExtractor
import soccerstand.parser.matchsummary.model.MatchEvent.MatchMinute
import soccerstand.util.Slf4jLogging

import scala.xml.Elem

object MatchCommentaryParser {

  import soccerstand.implicits.Implicits._

  def parseMatchCommentary(importantCommentsHtml: Elem): Seq[Commentary] = {
    val importantCommentsTrs = importantCommentsHtml \\ "tr"
    importantCommentsTrs.map { commentaryTr =>
      val tds = commentaryTr \\ "td"
      val minuteAndEventTd = tds(0)
      val commentaryTextTd = tds(1)
      val minute = TimeBoxExtractor.applyOpt(minuteAndEventTd).map(MatchMinute.fromString)
      val commentaryEvent = (minuteAndEventTd \\ "span").headOption.flatMap { eventSpan =>
        val spanClass = eventSpan \@ "class"
        val eventName = spanClass.replaceFirst("icon", "").withoutWhitespaces
        CommentaryEvent.fromString(eventName)
      }
      Commentary(minute, commentaryEvent, commentaryTextTd.text)
    }
  }

  sealed abstract class CommentaryEvent(val eventName: String)
  object CommentaryEvent extends Slf4jLogging {
    def fromString(s: String): Option[CommentaryEvent] = {
      s match {
        case CommentaryEvents.Whistle.eventName =>          Some(CommentaryEvents.Whistle)
        case CommentaryEvents.Goal.eventName =>             Some(CommentaryEvents.Goal)
        case CommentaryEvents.CornerKick.eventName =>       Some(CommentaryEvents.CornerKick)
        case CommentaryEvents.YellowCard.eventName =>       Some(CommentaryEvents.YellowCard)
        case CommentaryEvents.SecondYellowCard.eventName => Some(CommentaryEvents.SecondYellowCard)
        case CommentaryEvents.RedCard.eventName =>          Some(CommentaryEvents.RedCard)
        case CommentaryEvents.StoppageTime.eventName =>     Some(CommentaryEvents.StoppageTime)
        case CommentaryEvents.Injury.eventName =>           Some(CommentaryEvents.Injury)
        case CommentaryEvents.Substitution.eventName =>     Some(CommentaryEvents.Substitution)
        case CommentaryEvents.FunFact.eventName =>          Some(CommentaryEvents.FunFact)
        case CommentaryEvents.Penalty.eventName =>          Some(CommentaryEvents.Penalty)
        case "lineup1" => None
        case other =>
          warn(s"Unknown commentary event found: $other")
          None
      }
    }
  }
  object CommentaryEvents {
    case object Whistle extends CommentaryEvent("whistle")
    case object Goal extends CommentaryEvent("soccer-ball")
    case object CornerKick extends CommentaryEvent("corner")
    case object YellowCard extends CommentaryEvent("y-card")
    case object SecondYellowCard extends CommentaryEvent("yr-card")
    case object RedCard extends CommentaryEvent("r-card")
    case object StoppageTime extends CommentaryEvent("time")
    case object Injury extends CommentaryEvent("injury")
    case object Substitution extends CommentaryEvent("substitution")
    case object Penalty extends CommentaryEvent("penalty")
    case object FunFact extends CommentaryEvent("funfact")
  }

  case class Commentary(minute: Option[MatchMinute], event: Option[CommentaryEvent], text: String)
  sealed trait CommentaryFetchMode
  object CommentaryFetchModes {
    case object AllComments extends CommentaryFetchMode
    case object ImportantCommentsOnly extends CommentaryFetchMode
  }
  case class MatchCommentary(league: League, matchInfo: Match, commentary: Seq[Commentary])
}
