package soccerstand.model

case class LeagueScores(league: League, games: Seq[Match])

case class TodayScores(content: Seq[LeagueScores])