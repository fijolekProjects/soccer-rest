package soccerstand.parser

import soccerstand.model.{ClubStanding, League, LeagueStandings}
import soccerstand.parser.token.SoccerstandTokens.anyContent

import scala.xml.XML

object SoccerstandLeagueStandingsParser {

  def parseLeagueStandings(league: League, leagueHtmlData: String): LeagueStandings = {
    val allTdTagsPattern = "<td.*".r
    val splittedByClubs = allTdTagsPattern.findAllMatchIn(leagueHtmlData).map(_.toString()).toList
    val standings = splittedByClubs.map { clubData =>
      val tdTagPattern = s"(?i)<td([^>]+)>($anyContent)</td>".r
      val clubInfo = tdTagPattern.findAllMatchIn(clubData).toList
      val clubHtmlData = XML.loadString("<div>" + clubInfo.mkString("\n") + "</div>")
      val clubDataExtracted = (clubHtmlData \\ "td").take(8).map(_.text).toVector
      ClubStanding.fromTdValues(clubDataExtracted)
    }.toList
    LeagueStandings(league, standings)
  }
}