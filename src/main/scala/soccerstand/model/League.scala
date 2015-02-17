package soccerstand.model

import soccerstand.implicits.Implicits._
import soccerstand.parser.token.SoccerstandTokens._

case class Country(name: String, code: Int)
case class League(country: Country, leagueName: String) {
  def soccerstandResultsUrlPart: String =
    country.name.toLowerCase.whitespacesToDashes + "/" + leagueName.toLowerCase.withoutClausura.whitespacesToDashes

}

object League {
  def fromString(leagueStr: String): League = {
    val (countryName, leagueName) = leagueStr.takeTillEndChar.splitAt(leagueStr.indexOf(':'))
    val countryCodePattern = s"$countryCode(.+?)¬".r
    val parsedCountryCode = countryCodePattern.findFirstMatchIn(leagueStr).get.group(1).toInt
    League(Country(countryName, parsedCountryCode), leagueName.drop(2))
  }
}

