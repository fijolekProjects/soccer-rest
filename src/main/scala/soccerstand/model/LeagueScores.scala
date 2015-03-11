package soccerstand.model

case class TodayScores(content: Seq[LeagueScores])

case class LeagueScores(league: League, matches: Seq[Match])
