package soccerstand.parser.matchsummary.model

import soccerstand.model.{League, Match}
import soccerstand.parser.matchsummary.model.MatchEvent.MatchEvents

case class MatchSummary(league: League, matchInfo: Match, matchEvents: MatchEvents)
