package soccerstand.parser.matchsummary.extractors

import soccerstand.implicits.Implicits._
import soccerstand.parser.matchsummary.MatchSummaryParser._

import scala.xml.Node

case class HtmlEvent(name: String, classesWithData: List[String], classWithOptionalData: Option[String] = None)
case class PenaltyHtmlEventNames(className: String, comment: String)

trait CapableOfMatchEventExtracting[Event <: MatchEvent] {
  def unapply(matchEvent: Node): Option[Event]
}

trait MatchEventExtractor[Event <: MatchEvent] extends CapableOfMatchEventExtracting[Event] {
  protected def htmlEvent: HtmlEvent
  protected def mapEventData(textFromEventInfoClasses: List[String],
                              textFromClassWithOptionalData: Option[String],
                              minute: MatchMinute): Event

  def unapply(matchEvent: Node): Option[Event] = {
    val spans = matchEvent \\ "span"
    if (spans.map(_ \@ "class").containsElemWithPartOf(htmlEvent.name)) {
      val textFromEventInfoClasses = htmlEvent.classesWithData.map { spans.getTextFromClass }
      val optionalTextFromEventInfoClasses = htmlEvent.classWithOptionalData.flatMap { spans.findTextFromClass }
      Some(mapEventData(textFromEventInfoClasses, optionalTextFromEventInfoClasses, MatchMinute.fromMatchEvent(matchEvent)))
    } else None
  }
}

trait PenaltyEventExtractor[PenaltyEventType <: PenaltyMatchEvent] extends CapableOfMatchEventExtracting[PenaltyEventType]{
  protected def penaltyEventNames: PenaltyHtmlEventNames
  protected def constructor: (String, MatchMinute) => PenaltyEventType

  def unapply(matchEvent: Node): Option[PenaltyEventType] = {
    val spans = matchEvent \\ "span"
    if (spans.map(_ \@ "class").containsElemWithPartOf(penaltyEventNames.className) && matchEvent.text.contains(penaltyEventNames.comment)) {
      val player = spans.getTextFromClass("participant-name").withoutWhitespacesAtFrontAndBack
      Some(constructor(player, MatchMinute.fromMatchEvent(matchEvent)))
    } else None
  }
}

trait MatchEventExtractorWithoutOptionalFields[Event <: MatchEvent] extends MatchEventExtractor[Event] {
  override protected def mapEventData(textFromEventInfoClasses: List[String],
                                       optionalTextFromEventInfoClasses: Option[String],
                                       minute: MatchMinute): Event = {
    mapEventData(textFromEventInfoClasses, minute)
  }
  protected def mapEventData(textFromEventInfoClasses: List[String], minute: MatchMinute): Event
}

trait CardGivenExtractor[CardGivenMatchEvent <: MatchEvent] extends MatchEventExtractorWithoutOptionalFields[CardGivenMatchEvent] {
  protected val htmlCardEventName: String
  protected type Player = String
  protected type Reason = String
  protected val constructor: (Player, Reason, MatchMinute) => CardGivenMatchEvent
  override protected def htmlEvent = HtmlEvent(htmlCardEventName, List("participant-name", "subincident-penalty"))
  override protected def mapEventData(textFromEventInfoClasses: List[String], minute: MatchMinute): CardGivenMatchEvent = {
    val player :: reason :: Nil = textFromEventInfoClasses
    constructor(player.withoutWhitespacesAtFrontAndBack, reason.withoutParens, minute)
  }
}
