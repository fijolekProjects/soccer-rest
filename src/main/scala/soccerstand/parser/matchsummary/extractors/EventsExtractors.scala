package soccerstand.parser.matchsummary.extractors

import soccerstand.parser.matchsummary.MatchSummaryParser._

object EventsExtractors {
  import soccerstand.implicits.Implicits._

  object YellowCardExtractor extends CardGivenExtractor[YellowCard] {
    override protected val cardEventName: String = "y-card"
    override protected def cardMatchEventConstructor: (Guilty, Reason, MatchMinute) => YellowCard = YellowCard.apply
  }

  object SecondYellowCardExtractor extends CardGivenExtractor[SecondYellowCard] {
    override protected val cardEventName: String = "yr-card"
    override protected def cardMatchEventConstructor: (Guilty, Reason, MatchMinute) => SecondYellowCard = SecondYellowCard.apply
  }

  object SubstitutionExtractor extends MatchEventExtractorWithoutOptionalFields[Substitution] {
    override protected def htmlEvent: HtmlEvent = HtmlEvent("substitution-in", List("substitution-in-name", "substitution-out-name"))
    override protected def mapEventInfos(textFromEventInfoClasses: List[String], minute: MatchMinute): Substitution = {
      val playerIn :: playerOut :: Nil = textFromEventInfoClasses
      Substitution(playerIn.withoutWhitespacesAtFrontAndBack, playerOut.withoutWhitespacesAtFrontAndBack, minute)
    }
  }

  object GoalExtractor extends MatchEventExtractor[Goal] {
    override protected def htmlEvent: HtmlEvent = HtmlEvent("soccer-ball", List("participant-name"), List("assist"))
    override protected def mapEventInfos(textFromEventInfoClasses: List[String],
                                         optionalTextFromEventInfoClasses: List[Option[String]],
                                         minute: MatchMinute): Goal = {
      val scorer :: Nil = textFromEventInfoClasses
      val assist :: Nil = optionalTextFromEventInfoClasses
      Goal(scorer.withoutWhitespacesAtFrontAndBack, assist.map(_.withoutWhitespacesAtFrontAndBack), minute)
    }
  }

  object MissedPenaltyExtractor extends MatchEventExtractorWithoutOptionalFields[MissedPenalty] {
    override protected def htmlEvent: HtmlEvent = HtmlEvent("penalty-missed", List("participant-name"))
    override protected def mapEventInfos(textFromEventInfoClasses: List[String], minute: MatchMinute): MissedPenalty = {
      val unluckyFellow :: Nil = textFromEventInfoClasses
      MissedPenalty(unluckyFellow.withoutWhitespacesAtFrontAndBack, minute)
    }
  }
}
