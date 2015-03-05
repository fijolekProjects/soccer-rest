package soccerstand.parser.matchsummary.extractors

import soccerstand.implicits.Implicits._
import soccerstand.parser.matchsummary.MatchSummaryParser.{MatchMinute, MatchEvent}

import scala.xml.Node

case class HtmlEvent(name: String, classesWithData: List[String], classesWithOptionalData: List[String] = Nil)

trait MatchEventExtractor[Event <: MatchEvent] {
  protected def htmlEvent: HtmlEvent
  protected def mapEventInfos(textFromEventInfoClasses: List[String],
                              optionalTextFromEventInfoClasses: List[Option[String]],
                              minute: MatchMinute): Event

  def unapply(matchEvent: Node): Option[Event] = {
    val spans = matchEvent \\ "span"
    if (spans.map(_ \@ "class").containsElemWithPartOf(htmlEvent.name)) {
      val minute = (matchEvent \\ "div").getTextFromClass("time-box").init
      val matchMinute = MatchMinute.fromString(minute)
      val textFromEventInfoClasses = htmlEvent.classesWithData.map { spans.getTextFromClass }
      val optionalTextFromEventInfoClasses = htmlEvent.classesWithOptionalData.map { spans.findTextFromClass }
      Some(mapEventInfos(textFromEventInfoClasses, optionalTextFromEventInfoClasses, matchMinute))
    } else None
  }
}

trait MatchEventExtractorWithoutOptionalFields[Event <: MatchEvent] extends MatchEventExtractor[Event] {
  override protected def mapEventInfos(textFromEventInfoClasses: List[String],
                                       optionalTextFromEventInfoClasses: List[Option[String]],
                                       minute: MatchMinute): Event = {
    mapEventInfos(textFromEventInfoClasses, minute)
  }
  protected def mapEventInfos(textFromEventInfoClasses: List[String], minute: MatchMinute): Event
}

trait CardGivenExtractor[CardGivenMatchEvent <: MatchEvent] extends MatchEventExtractorWithoutOptionalFields[CardGivenMatchEvent] {
  protected val cardEventName: String
  protected type Guilty = String
  protected type Reason = String
  protected def cardMatchEventConstructor: (Guilty, Reason, MatchMinute) => CardGivenMatchEvent
  override protected def htmlEvent = HtmlEvent(cardEventName, List("participant-name", "subincident-penalty"))
  override protected def mapEventInfos(textFromEventInfoClasses: List[String], minute: MatchMinute): CardGivenMatchEvent = {
    val guilty :: reason :: Nil = textFromEventInfoClasses
    cardMatchEventConstructor(guilty.withoutWhitespacesAtFrontAndBack, reason.withoutParens, minute)
  }
}
