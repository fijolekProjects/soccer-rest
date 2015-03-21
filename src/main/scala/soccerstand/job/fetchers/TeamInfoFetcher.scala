package soccerstand.job.fetchers

import soccerstand.model._
import soccerstand.service.communication.SoccerstandCommunication._
import soccerstand.util.{Slf4jLogging, Measureable}

class TeamInfoFetcher extends Slf4jLogging with Measureable {
  import soccerstand.parser.token.SoccerstandTokens._

  def fetchTeams(leagues: Seq[LeagueInfo]): Seq[TeamInfo] = {
    leagues.par.flatMap { leagueInfo =>
      val league = leagueInfo.league
      infoBlock(s"fetching teams for $league") {
        fetchTeamsFromLeague(league)
      }
    }.seq
  }

  private def fetchTeamsFromLeague(league: League): Seq[TeamInfo] = {
    val urlPart = league.soccerstandResultsUrlPart
    val teamsHtmlForLeague = scala.io.Source.fromURL(s"http://$soccerstandFrontendRoute/soccer/$urlPart/teams/").mkString
    val teamIdAndNamePattern = s"""href="/team/.+?/($anyContent)">($anyContent)<""".r
    teamIdAndNamePattern.findAllMatchIn(teamsHtmlForLeague).map { matched =>
      val teamId = SoccerstandTeamId(matched.group(1))
      val teamName = matched.group(2)
      //DOIT team needs league, and team info also needs league? straight it up
      val team = Team(teamName, league)
      TeamInfo(teamId, team, league)
    }.toSeq
  }
}