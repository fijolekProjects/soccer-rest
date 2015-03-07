package soccerstand.parser.token

object SoccerstandTokens {
  type Token = String
  val homeClub = "CX÷"
  val homeClubScore = "AG÷"
  val homeClubScoreFromMatchId = "DE÷"
  val awayClub = "AF÷"
  val awayClubScore = "AH÷"
  val awayClubScoreFromMatchId = "DF÷"
  val gameStatus = "AB÷"
  val gameStatusFromMatchId = "DA÷"
  val startDate = "AD÷"
  val startDateFromMatchId = "DC÷"
  val newLeague = "~ZA÷"
  val endOfUsefulData1 = "QA÷1"
  val endOfUsefulData2 = "A1÷"
  val tournamentId = "ZE÷"
  val tournamentStageId = "ZC÷"
  val countryCode = "ZB÷"
  val round = "ER÷"
  val newGame = '~'
  val gameId = "AA÷"
  val anyContent = ".+?"
}

