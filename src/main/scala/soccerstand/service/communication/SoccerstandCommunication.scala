package soccerstand.service.communication

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.Http
import akka.http.client.RequestBuilding
import akka.http.model.headers.RawHeader
import akka.http.model.{HttpRequest, HttpResponse}
import akka.stream.scaladsl.Source
import soccerstand.model.{LeagueInfo, TournamentIds}
import soccerstand.service.communication.SoccerstandCommunication._

class SoccerstandCommunication(val logger: LoggingAdapter)(implicit system: ActorSystem) {

  def todaySource: Source[HttpResponse] = {
    SoccerstandRequest.Get("/x/feed/f_1_0_1_en_1/")
  }

  def standingsSource(tournamentIds: TournamentIds): Source[HttpResponse] = {
    val tournamentPart = buildTournamentRequestPart(tournamentIds)
    SoccerstandRequest.Get(s"/x/feed/ss_4_${tournamentPart}_table_overall/")
  }
  
  def topScorersSource(tournamentIds: TournamentIds): Source[HttpResponse] = {
    val tournamentPart = buildTournamentRequestPart(tournamentIds)
    SoccerstandRequest.Get(s"/x/feed/ss_1_${tournamentPart}_top_scorers_")
  }

  private def buildTournamentRequestPart(tournamentIds: TournamentIds): String = {
    s"${tournamentIds.tournamentIdString}_${tournamentIds.tournamentStageId}"
  }

  def todayLeagueResultsSource(leagueInfo: LeagueInfo): Source[HttpResponse] = {
    SoccerstandRequest.Get(s"/x/feed/t_1_${leagueInfo.countryCode}_${leagueInfo.leagueId}_1_en_1/")
  }
  
  def latestLeagueResultsSource(leagueInfo: LeagueInfo): Source[HttpResponse] = {
    val urlPath = s"/x/feed/tr_1_${leagueInfo.countryCode}_${leagueInfo.leagueId}_${leagueInfo.seasonId}_0_1_en_1/"
    SoccerstandRequest.Get(urlPath)
  }

  object SoccerstandRequest {
    val Get = GetDef _
    private def GetDef(uri: String): Source[HttpResponse] = {
      val req = RequestBuilding.Get(uri)
      val reqWithHeader = req.addHeader(RawHeader("X-Fsign", "SW9D1eZo"))
      val route = soccerstandBackendRoute
      logger.info(s"external service request: $route${reqWithHeader.uri}")
      Source.single(reqWithHeader).via(Http().outgoingConnection(route).flow)
    }
  }
}

object SoccerstandCommunication {
  private val servers = Seq(
    "soccerstand.com",
    "flashscore.com"
  )
  def soccerstandBackendRoute = "d." + scala.util.Random.shuffle(servers).head
  def soccerstandFrontend = scala.util.Random.shuffle(servers).head
}