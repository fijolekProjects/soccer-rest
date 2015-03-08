package soccerstand.model

import db.ConvertableToDBObject
import soccerstand.implicits.Implicits._
import soccerstand.indexes.TournamentIdsIndexes
import soccerstand.model.LeagueInfo._

case class LeagueInfo(league: League, tournamentIds: TournamentIds, tournamentNumIds: TournamentNumIds) extends ConvertableToDBObject {
  val countryCode = league.country.code
  val leagueId = tournamentNumIds.tournamentId
  val seasonId = tournamentNumIds.tournamentPageSeasonResults
  val naturalId = createNaturalId(league.country.name, leagueName)
  def countryName = league.country.name
  def leagueName = league.leagueName
}
object LeagueInfo {
  def createNaturalId(country: String, leagueName: String): String = {
    country.withoutWhitespaces.toLowerCase + leagueName.toLowerCase.normalizedLeagueName
  }
  def soccerstandResultsUrlPart(leagueInfo: LeagueInfo): String = leagueInfo.league.soccerstandResultsUrlPart
}

case class TournamentIds(tournamentIdString: String, tournamentStageId: String)
object TournamentIds {
  def fromString(leagueInfoToParse: String, leagueInfoIndexes: TournamentIdsIndexes): TournamentIds = {
    val tournamentId = leagueInfoToParse.readDataAfterIdx(leagueInfoIndexes.tournamentIdIdx)
    val tournamentStageId = leagueInfoToParse.readDataAfterIdx(leagueInfoIndexes.tournamentStageIdIdx)
    TournamentIds(tournamentId, tournamentStageId)
  }
  import soccerstand.parser.token.SoccerstandTokens._

  def fromMatchHtmlPage(matchHtmlPage: String) = {
    val tournamentIdPat = s"tournamentEncodedId = '($anyContent)';".r
    val tournamentStageIdPat = s"tournamentStageEncodedId = '($anyContent)';".r
    val tournamentId = tournamentIdPat.findFirstMatchIn(matchHtmlPage).get.group(1)
    val tournamentStageId = tournamentStageIdPat.findFirstMatchIn(matchHtmlPage).get.group(1)
    TournamentIds(tournamentId, tournamentStageId)
  }
}
case class TournamentNumIds(tournamentId: Int, tournamentPageSeasonResults: Int)
object TournamentNumIds {
  val zero = TournamentNumIds(-1, -1)
}