package soccerstand.parser.matchstats

import soccerstand.model.{League, Match}
import soccerstand.parser.token.SoccerstandTokens._
import soccerstand.util.enum.ReflectiveEnum

import scala.xml.XML

object MatchStatisticsParser {
  import soccerstand.implicits.Implicits._

  def parseMatchStatistics(matchHtmlStats: String): Stats = {
    val divsWithMatchStatsPattern = s"""<div id="tab-statistics-[0-3]-statistic"$anyContent>($anyContent)</table></div>""".r
    val divsWithMatchStats = divsWithMatchStatsPattern.findAllMatchIn(matchHtmlStats).toSeq
    val divsWithMatchStatsSafe = divsWithMatchStats.map(_.toString()).lift
    val (overallStatsData, firstHalfStatsData, secondHalfStatsData, extraTimeHalfStatsData) =
      (divsWithMatchStatsSafe(0), divsWithMatchStatsSafe(1), divsWithMatchStatsSafe(2), divsWithMatchStatsSafe(3))
    val overallStats = overallStatsData.toSeq.flatMap(makeMatchStatsTyped)
    val firstHalfStats = firstHalfStatsData.toSeq.flatMap(makeMatchStatsTyped)
    val secondHalfStats = secondHalfStatsData.toSeq.flatMap(makeMatchStatsTyped)
    val extraTimeStats = extraTimeHalfStatsData.toSeq.flatMap(makeMatchStatsTyped)
    Stats(overallStats, firstHalfStats, secondHalfStats, extraTimeStats)
  }

  private def makeMatchStatsTyped(matchStatsData: String): Seq[MatchStat] = {
    val xml = XML.loadString(matchStatsData)
    val allRows = xml \\ "tr"
    allRows.map { row =>
      val tdsInsideRow = row \\ "td"
      val statName = tdsInsideRow.getTextFromClass("score stats")
      val typedStatName = MatchStatNames.getByName(statName)
      val homeTeamStatValue = tdsInsideRow.getTextFromClass(homeTeamMark).withoutPercents.toInt
      val awayTeamStatValue = tdsInsideRow.getTextFromClass(awayTeamMark).withoutPercents.toInt
      MatchStat(typedStatName, homeTeamStatValue, awayTeamStatValue)
    }
  }
}
case class MatchStatistics(league: League, matchInfo: Match, stats: Stats)
case class Stats(overall: Seq[MatchStat], firstHalf: Seq[MatchStat], secondHalf: Seq[MatchStat], extraTime: Seq[MatchStat])

case class MatchStat(name: MatchStatName, homeTeam: Int, awayTeam: Int)
sealed abstract class MatchStatName(val statName: String)
object MatchStatNames extends ReflectiveEnum[MatchStatName] {
  case object BallPossession    extends MatchStatName("Ball Possession")
  case object GoalAttempts      extends MatchStatName("Goal Attempts")
  case object ShotsOnGoal       extends MatchStatName("Shots on Goal")
  case object ShotsOffGoal      extends MatchStatName("Shots off Goal")
  case object BlockedShots      extends MatchStatName("Blocked Shots")
  case object FreeKicks         extends MatchStatName("Free Kicks")
  case object CornerKicks       extends MatchStatName("Corner Kicks")
  case object Offsides          extends MatchStatName("Offsides")
  case object ThrowIn           extends MatchStatName("Throw-in")
  case object GoalkeeperSaves   extends MatchStatName("Goalkeeper Saves")
  case object Fouls             extends MatchStatName("Fouls")
  case object RedCards          extends MatchStatName("Red Cards")
  case object YellowCards       extends MatchStatName("Yellow Cards")

  def getByName(statName: String) = MatchStatNames.values.find { _.statName.toLowerCase == statName.toLowerCase }.get
}