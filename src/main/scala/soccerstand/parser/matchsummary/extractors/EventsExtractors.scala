package soccerstand.parser.matchsummary.extractors

import soccerstand.parser.matchsummary.model.MatchEvent._

object EventsExtractors {
  import soccerstand.implicits.Implicits._

  object YellowCardExtractor extends CardGivenExtractor[YellowCard] {
    override protected val htmlCardEventName = "y-card"
    override protected val constructor = YellowCard.apply _
  }

  object SecondYellowCardExtractor extends CardGivenExtractor[SecondYellowCard] {
    override protected val htmlCardEventName = "yr-card"
    override protected val constructor = SecondYellowCard.apply _
  }

  object RedCardExtractor extends CardGivenExtractor[RedCard] {
    override protected val htmlCardEventName = "r-card"
    override protected val constructor = RedCard.apply _
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

  object OwnGoalExtractor extends MatchEventExtractorWithoutOptionalFields[OwnGoal] {
    override protected val htmlEvent = HtmlEvent("soccer-ball-own", List("participant-name"))
    override protected def mapEventData(textFromEventInfoClasses: List[String], minute: MatchMinute): OwnGoal = {
      val scorer :: Nil = textFromEventInfoClasses
      OwnGoal(scorer.withoutWhitespacesAtFrontAndBack, minute)
    }
  }

  object ScoredPenaltyExtractor extends PenaltyEventExtractor[ScoredPenalty] {
    override protected val penaltyEventNames = PenaltyExtractors.scoredPenaltyEventNames
    override protected val constructor = ScoredPenalty.apply _
  }

  object MissedPenaltyExtractor extends PenaltyEventExtractor[MissedPenalty] {
    override protected val penaltyEventNames = PenaltyExtractors.missedPenaltyEventNames
    override protected val constructor = MissedPenalty.apply _
  }

  object OffMatchScoredPenaltyExtractor extends OffMatchPenaltyEventExtractor[OffMatchScoredPenalty] {
    override protected val penaltyEventNames = PenaltyExtractors.scoredPenaltyEventNames
    override protected val constructor = OffMatchScoredPenalty.apply _
  }

  object OffMatchMissedPenaltyExtractor extends OffMatchPenaltyEventExtractor[OffMatchMissedPenalty] {
    override protected val penaltyEventNames = PenaltyExtractors.missedPenaltyEventNames
    override protected val constructor = OffMatchMissedPenalty.apply _
  }

  object PenaltyExtractors {
    val scoredPenaltyEventNames = PenaltyHtmlEventNames("soccer-ball", "(Penalty)")
    val missedPenaltyEventNames = PenaltyHtmlEventNames("penalty-missed", "(Penalty missed)")
  }

}
