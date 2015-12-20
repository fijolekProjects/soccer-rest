package soccerstand.service.communication

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding
import akka.http.scaladsl.model.{ResponseEntity, HttpResponse}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import soccerstand.model.{TeamInfo, LeagueInfo, TournamentIds}
import soccerstand.service.communication.SoccerstandCommunication._

import scala.concurrent.{ExecutionContext, Future}

class SoccerstandCommunication(val logger: LoggingAdapter)(implicit system: ActorSystem) {

  def todaySource = SoccerstandRequest.GetBE("/x/feed/f_1_0_1_en_1/")

  def standingsSource(tournamentIds: TournamentIds) = {
    val tournamentPart = buildTournamentRequestPart(tournamentIds)
    SoccerstandRequest.GetBE(s"/x/feed/ss_4_${tournamentPart}_table_overall/")
  }
  
  def topScorersSource(tournamentIds: TournamentIds) = {
    val tournamentPart = buildTournamentRequestPart(tournamentIds)
    SoccerstandRequest.GetBE(s"/x/feed/ss_1_${tournamentPart}_top_scorers_")
  }

  private def buildTournamentRequestPart(tournamentIds: TournamentIds): String = {
    s"${tournamentIds.tournamentIdString}_${tournamentIds.tournamentStageId}"
  }

  def todayLeagueResultsSource(leagueInfo: LeagueInfo) = {
    SoccerstandRequest.GetBE(s"/x/feed/t_1_${leagueInfo.countryCode}_${leagueInfo.leagueId}_1_en_1/")
  }
  
  def latestLeagueResultsSource(leagueInfo: LeagueInfo) = {
    val urlPath = s"/x/feed/tr_1_${leagueInfo.countryCode}_${leagueInfo.leagueId}_${leagueInfo.seasonId}_0_1_en_1/"
    SoccerstandRequest.GetBE(urlPath)
  }

  def latestTeamResultsSource(teamInfo: TeamInfo) = {
    val urlPath = s"/x/feed/pr_1_${teamInfo.league.country.code}_${teamInfo.id.value}_0_1_en_1/"
    SoccerstandRequest.GetBE(urlPath)
  }

  def matchSummarySource(matchId: String) = {
    SoccerstandRequest.GetBE(s"/x/feed/d_su_${matchId}_en_1/")
  }

  def matchDetailsSource(matchId: String) = {
    SoccerstandRequest.GetBE(s"/x/feed/dc_1_$matchId/")
  }

  def matchHtmlSource(matchId: String) = {
    SoccerstandRequest.GetFE(s"/match/$matchId/")
  }

  def teamsHtmlSource(leagueInfo: LeagueInfo) = {
    SoccerstandRequest.GetFE(s"/soccer/${leagueInfo.countryName}/${leagueInfo.leagueName}/teams/")
  }

  def matchHtmlStatistics(matchId: String) = {
    SoccerstandRequest.GetBE(s"/x/feed/d_st_${matchId}_en_1/")
  }

  object SoccerstandRequest {
    val GetBE = getRequest(soccerstandBackendRoute) _
    val GetFE = getRequest(soccerstandFrontendRoute) _
    private def getRequest(route: String)(uri: String): SoccerstandSource = {
      val req = RequestBuilding.Get(uri)
      val reqWithHeader = req.addHeader(RawHeader("X-Fsign", "SW9D1eZo"))
      logger.info(s"external service request: $route${reqWithHeader.uri}")
      val source = Source.single(reqWithHeader).via(Http().outgoingConnection(route))
      SoccerstandSource(source)
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

case class SoccerstandSource(source: Source[HttpResponse, Unit]) extends AnyVal {
  def fetchSoccerstandData[T](mapResponse: String => T)
                             (implicit mat: Materializer,
                              ec: ExecutionContext,
                              um: Unmarshaller[ResponseEntity, String]): Future[T] = {
    source.runWith(Sink.head).flatMap { response =>
      if (response.status.isSuccess()) {
          Unmarshal(response.entity).to[String].map { mapResponse }
      } else {
          throw new RuntimeException(s"unexpected happenned: ${response.status}")
      }
    }
  }
}
