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

  sealed trait CommentaryEvent
  object CommentaryEvent extends Slf4jLogging {
    def fromString(s: String): Option[CommentaryEvent] = {
      s match {
        case "whistle" => Some(CommentaryEvents.Whistle)
        case "soccer-ball" => Some(CommentaryEvents.Goal)
        case "corner" => Some(CommentaryEvents.CornerKick)
        case "y-card" => Some(CommentaryEvents.YellowCard)
        case "yr-card" => Some(CommentaryEvents.SecondYellowCard)
        case "r-card" => Some(CommentaryEvents.RedCard)
        case "time" => Some(CommentaryEvents.StoppageTime)
        case "injury" => Some(CommentaryEvents.Injury)
        case "substitution" => Some(CommentaryEvents.Substitution)
        case "funfact" => Some(CommentaryEvents.FunFact)
        case "penalty" => Some(CommentaryEvents.Penalty)
        case "lineup1" => None
        case other =>
          warn(s"Unknown commentary event found: $other")
          None
      }
    }
  }
  object CommentaryEvents {
    case object Whistle extends CommentaryEvent
    case object Goal extends CommentaryEvent
    case object CornerKick extends CommentaryEvent
    case object YellowCard extends CommentaryEvent
    case object SecondYellowCard extends CommentaryEvent
    case object RedCard extends CommentaryEvent
    case object StoppageTime extends CommentaryEvent
    case object Injury extends CommentaryEvent
    case object Substitution extends CommentaryEvent
    case object Penalty extends CommentaryEvent
    case object FunFact extends CommentaryEvent
  }

  case class Commentary(minute: Option[MatchMinute], event: Option[CommentaryEvent], text: String)
  sealed trait CommentaryFetchMode
  object CommentaryFetchModes {
    case object AllComments extends CommentaryFetchMode
    case object ImportantCommentsOnly extends CommentaryFetchMode
  }
  case class MatchCommentary(league: League, matchInfo: Match, commentary: Seq[Commentary])
}
