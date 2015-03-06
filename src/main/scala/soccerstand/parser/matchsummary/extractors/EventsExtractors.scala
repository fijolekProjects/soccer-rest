package soccerstand.parser.matchsummary.extractors

import soccerstand.parser.matchsummary.model.MatchEvent._

object EventsExtractors {
  import soccerstand.implicits.Implicits._

  object YellowCardExtractor extends CardGivenExtractor[YellowCard] {
    override protected val htmlCardEventName = "y-card"
    override protected val constructor: (Player, Reason, MatchMinute) => YellowCard = YellowCard.apply
  }

  object SecondYellowCardExtractor extends CardGivenExtractor[SecondYellowCard] {
    override protected val htmlCardEventName = "yr-card"
    override protected val constructor: (Player, Reason, MatchMinute) => SecondYellowCard = SecondYellowCard.apply
  }

  object SubstitutionExtractor extends MatchEventExtractorWithoutOptionalFields[Substitution] {
    override protected val htmlEvent = HtmlEvent("substitution-in", List("substitution-in-name", "substitution-out-name"))
    override protected def mapEventData(textFromEventInfoClasses: List[String], minute: MatchMinute): Substitution = {
      val playerIn :: playerOut :: Nil = textFromEventInfoClasses
      Substitution(playerIn.withoutWhitespacesAtFrontAndBack, playerOut.withoutWhitespacesAtFrontAndBack, minute)
    }
  }

  object GoalExtractor extends MatchEventExtractor[Goal] {
    override protected val htmlEvent = HtmlEvent("soccer-ball", List("participant-name"), Some("assist"))
    override protected def mapEventData(textFromEventInfoClasses: List[String],
                                        assist: Option[String],
                                        minute: MatchMinute): Goal = {
      val scorer :: Nil = textFromEventInfoClasses
      Goal(scorer.withoutWhitespacesAtFrontAndBack, assist.map(_.withoutParens), minute)
    }
  }

  object ScoredPenaltyExtractor extends PenaltyEventExtractor[ScoredPenalty] {
    override protected val penaltyEventNames = PenaltyHtmlEventNames("soccer-ball", "(Penalty)")
    override protected val constructor: (String, MatchMinute) => ScoredPenalty = ScoredPenalty.apply
  }

  object MissedPenaltyExtractor extends PenaltyEventExtractor[MissedPenalty] {
    override protected val penaltyEventNames = PenaltyHtmlEventNames("penalty-missed", "(Penalty missed)")
    override protected val constructor: (String, MatchMinute) => MissedPenalty = MissedPenalty.apply
  }
}
