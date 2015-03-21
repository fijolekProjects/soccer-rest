package soccerstand.job

import db.repository.{TeamInfoRepository, LeagueInfoRepository}
import soccerstand.job.fetchers.{TeamInfoFetcher, LeagueInfoFetcher}
import soccerstand.model.{TeamInfo, LeagueInfo}
import soccerstand.util.{Measureable, Slf4jLogging}

object SoccerstandDataSaver extends Slf4jLogging with Measureable {
  private val leagueInfoFetcher = new LeagueInfoFetcher()
  private val teamInfoFetcher = new TeamInfoFetcher()
  private val leagueInfoRepository = new LeagueInfoRepository()
  private val teamInfoRepository = new TeamInfoRepository()

  def main (args: Array[String]) {
    saveTeamsInfoOnly()
  }

  def saveAllDataOnEmptyDB(): Unit = {
    measure("job is done!") {
      val leagues = allLeagues
      val teamInfos = allTeamInfos(leagues)
      saveLeagues(leagues)
      saveTeams(teamInfos)
    }
  }

  def saveTeamsInfoOnly(): Unit = {
    val leagues = leagueInfoRepository.findAll()
    val teamInfos = allTeamInfos(leagues)
    saveTeams(teamInfos)
  }

  private def saveTeams(teamInfos: Seq[TeamInfo]): Unit = {
    measure(s"saving teams data for: ${teamInfos.size} teams") {
      teamInfoRepository.createOrUpdateAll(teamInfos)
    }
  }

  private def saveLeagues(leagues: Seq[LeagueInfo]): Unit = {
    measure(s"saving all base league data for ${leagues.size} leagues") {
      leagueInfoRepository.createOrUpdateAll(leagues)
    }
  }

  private def allTeamInfos(leagues: Seq[LeagueInfo]): Seq[TeamInfo] = {
    measure("fetching info for teams from all leagues") {
      teamInfoFetcher.fetchTeams(leagues)
    }
  }

  private def allLeagues: Seq[LeagueInfo] = {
    measure("fetching info for leagues from all countries") {
      leagueInfoFetcher.fetchAllLeagues()
    }
  }

}
