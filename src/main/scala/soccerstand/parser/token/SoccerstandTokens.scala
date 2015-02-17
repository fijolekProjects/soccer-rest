package soccerstand.parser.token

object SoccerstandTokens {
  type Token = String
  val homeClub = "CX÷"
  val homeClubScore = "AG÷"
  val awayClub = "AF÷"
  val awayClubScore = "AH÷"
  val gameStatus = "AB÷"
  val startDate = "AD÷"
  val newLeague = "~ZA÷"
  val endOfUsefulData1 = "QA÷1"
  val endOfUsefulData2 = "A1÷"
  val tournamentId = "ZE÷"
  val tournamentStageId = "ZC÷"
  val countryCode = "ZB÷"
  val round = "ER÷"
  val newGame = '~'
}

