package soccerstand.parser.token

object SoccerstandTokens {
  type Token = String
  val homeTeam = "CX÷"
  val homeTeamScore = "AG÷"
  val homeTeamScoreFromMatchId = "DE÷"
  val awayTeam = "AF÷"
  val awayTeamScore = "AH÷"
  val awayTeamScoreFromMatchId = "DF÷"
  val matchStatus = "AB÷"
  val matchStatusFromMatchId = "DA÷"
  val startDate = "AD÷"
  val startDateFromMatchId = "DC÷"
  val newLeague = "~ZA÷"
  val endOfUsefulData1 = "QA÷1"
  val endOfUsefulData2 = "A1÷"
  val tournamentId = "ZE÷"
  val tournamentStageId = "ZC÷"
  val countryCode = "ZB÷"
  val round = "ER÷"
  val newMatch = '~'
  val matchId = "AA÷"
  val anyContent = ".+?"
  val homeTeamMark = "fl"
  val awayTeamMark = "fr"
}

