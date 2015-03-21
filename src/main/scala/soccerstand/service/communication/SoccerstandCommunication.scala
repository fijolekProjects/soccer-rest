package soccerstand.service.communication

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.Http
import akka.http.client.RequestBuilding
import akka.http.model.HttpResponse
import akka.http.model.headers.RawHeader
import akka.stream.scaladsl.Source
import soccerstand.model.{TeamInfo, LeagueInfo, TournamentIds}
import soccerstand.service.communication.SoccerstandCommunication._

class SoccerstandCommunication(val logger: LoggingAdapter)(implicit system: ActorSystem) {

  def todaySource: Source[HttpResponse, Unit] = SoccerstandRequest.GetBE("/x/feed/f_1_0_1_en_1/")

  def standingsSource(tournamentIds: TournamentIds): Source[HttpResponse, Unit] = {
    val tournamentPart = buildTournamentRequestPart(tournamentIds)
    SoccerstandRequest.GetBE(s"/x/feed/ss_4_${tournamentPart}_table_overall/")
  }
  
  def topScorersSource(tournamentIds: TournamentIds): Source[HttpResponse, Unit] = {
    val tournamentPart = buildTournamentRequestPart(tournamentIds)
    SoccerstandRequest.GetBE(s"/x/feed/ss_1_${tournamentPart}_top_scorers_")
  }

  private def buildTournamentRequestPart(tournamentIds: TournamentIds): String = {
    s"${tournamentIds.tournamentIdString}_${tournamentIds.tournamentStageId}"
  }

  def todayLeagueResultsSource(leagueInfo: LeagueInfo): Source[HttpResponse, Unit] = {
    SoccerstandRequest.GetBE(s"/x/feed/t_1_${leagueInfo.countryCode}_${leagueInfo.leagueId}_1_en_1/")
  }
  
  def latestLeagueResultsSource(leagueInfo: LeagueInfo): Source[HttpResponse, Unit] = {
    val urlPath = s"/x/feed/tr_1_${leagueInfo.countryCode}_${leagueInfo.leagueId}_${leagueInfo.seasonId}_0_1_en_1/"
    SoccerstandRequest.GetBE(urlPath)
  }

  def latestTeamResultsSource(teamInfo: TeamInfo): Source[HttpResponse, Unit] = {
    val urlPath = s"/x/feed/pr_1_${teamInfo.league.country.code}_${teamInfo.id.value}_0_1_en_1/"
    SoccerstandRequest.GetBE(urlPath)
  }

  def matchSummarySource(matchId: String): Source[HttpResponse, Unit] = {
    SoccerstandRequest.GetBE(s"/x/feed/d_su_${matchId}_en_1/")
  }

  def matchDetailsSource(matchId: String): Source[HttpResponse, Unit] = {
    SoccerstandRequest.GetBE(s"/x/feed/dc_1_$matchId/")
  }

  def matchHtmlSource(matchId: String): Source[HttpResponse, Unit] = {
    SoccerstandRequest.GetFE(s"/match/$matchId/")
  }

  def teamsHtmlSource(leagueInfo: LeagueInfo): Source[HttpResponse, Unit] = {
    SoccerstandRequest.GetFE(s"/soccer/${leagueInfo.countryName}/${leagueInfo.leagueName}/teams/")
  }

  object SoccerstandRequest {
    val GetBE = getRequest(soccerstandBackendRoute) _
    val GetFE = getRequest(soccerstandFrontendRoute) _
    private def getRequest(route: String)(uri: String): Source[HttpResponse, Unit] = {
      val req = RequestBuilding.Get(uri)
      val reqWithHeader = req.addHeader(RawHeader("X-Fsign", "SW9D1eZo"))
      logger.info(s"external service request: $route${reqWithHeader.uri}")
      Source.single(reqWithHeader).via(Http().outgoingConnection(route))
    }
  }
}

object SoccerstandCommunication {
  private val servers = Seq(
    "soccerstand.com"/*,
    "flashscore.com"*/
  )
  def soccerstandBackendRoute = "d." + scala.util.Random.shuffle(servers).head
  def soccerstandFrontendRoute = "www." + scala.util.Random.shuffle(servers).head
}