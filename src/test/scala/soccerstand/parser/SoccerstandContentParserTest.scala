package soccerstand.parser

import org.scalatest.FlatSpec

class SoccerstandContentParserTest extends FlatSpec {

  it should "parse match summary correctly" in {
    val matchSummaryHtml = scala.io.Source.fromFile("/home/mihas/projects/soccer-rest/soccerstand-site/matchSummary3.txt").mkString

    val matchSummary = SoccerstandContentParser.parseMatchSummary(matchSummaryHtml)

    val home = matchSummary.homeTeam
    val away = matchSummary.awayTeam

    println(home.mkString("\n"))
    println()
    println(away.mkString("\n"))
  }

}
