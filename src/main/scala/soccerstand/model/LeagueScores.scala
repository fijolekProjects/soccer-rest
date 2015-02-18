package soccerstand.model

case class LeagueScores(league: League, games: Seq[TodayGame])

case class TodayScores(content: Seq[LeagueScores])