package soccerstand.model

case class LeagueScores(league: League, matches: Seq[Match])

case class TodayScores(content: Seq[LeagueScores])