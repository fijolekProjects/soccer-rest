package soccerstand.parser.matchsummary.model

import soccerstand.model.Match
import soccerstand.parser.matchsummary.model.MatchEvent.MatchEvents

case class MatchSummary(matchInfo: Match, matchEvents: MatchEvents)
