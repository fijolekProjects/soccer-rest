package soccerstand.service.communication

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.Http
import akka.http.client.RequestBuilding
import akka.http.model.headers.RawHeader
import akka.http.model.{HttpRequest, HttpResponse}
import akka.stream.scaladsl.Source
import soccerstand.model.{LeagueInfo, TournamentIds}

class SoccerstandCommunication(val logger: LoggingAdapter)(implicit system: ActorSystem) {
  private val servers = Seq(
    "d.soccerstand.com",
    "d.flashscore.com",
    "d.soccer24.com"
  )
  private def soccerstandBackendRoute = scala.util.Random.shuffle(servers).head
  private def soccerstandFlow = Http().outgoingConnection(soccerstandBackendRoute).flow

  def liveSource: Source[HttpResponse] = {
    val soccerstandReq = withSoccerstandHeader { RequestBuilding.Get("/x/feed/f_1_0_1_en_1/") }
    Source.single(soccerstandReq).via(soccerstandFlow)
  }

  def standingsSource(tournamentIds: TournamentIds): Source[HttpResponse] = {
    val soccerstandReq = withSoccerstandHeader {
      RequestBuilding.Get(s"/x/feed/ss_4_${tournamentIds.tournamentIdString}_${tournamentIds.tournamentStageId}_table_overall/")
    }
    Source.single(soccerstandReq).via(soccerstandFlow)
  }

  def leagueResultsSource(leagueInfo: LeagueInfo): Source[HttpResponse] = {
    val backendRoute = soccerstandBackendRoute
    val soccerstandReq = withSoccerstandHeader {
      RequestBuilding.Get(s"/x/feed/t_1_${leagueInfo.countryCode}_${leagueInfo.leagueId}_1_en_1/")
    }
    //DOIT think about logging fetched urls
    logger.info(s"fetching data from: $backendRoute${soccerstandReq.uri}")
    Source.single(soccerstandReq).via(Http().outgoingConnection(backendRoute).flow)
  }

  def latestLeagueResultsSource(leagueInfo: LeagueInfo): Source[HttpResponse] = {
    val soccerstandReq = withSoccerstandHeader {
      RequestBuilding.Get(s"/x/feed/tr_1_${leagueInfo.countryCode}_${leagueInfo.leagueId}_${leagueInfo.seasonId}_0_1_en_1/")
    }
    Source.single(soccerstandReq).via(Http().outgoingConnection(soccerstandBackendRoute).flow)
  }

  private def withSoccerstandHeader(req: => HttpRequest): HttpRequest = {
    req.addHeader(RawHeader("X-Fsign", "SW9D1eZo"))
  }
}