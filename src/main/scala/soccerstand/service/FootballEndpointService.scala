package soccerstand.service

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.Http
import akka.http.marshallers.sprayjson.SprayJsonSupport._
import akka.http.marshalling.ToResponseMarshallable
import akka.http.model.HttpResponse
import akka.http.model.StatusCodes._
import akka.http.server.Directives._
import akka.http.unmarshalling.Unmarshal
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorFlowMaterializer, FlowMaterializer}
import com.typesafe.config.ConfigFactory
import db.{DBFactory, LeagueInfoRepository}
import soccerstand.dto.FinishedGamesDto.LatestFinishedGamesDto
import soccerstand.dto.GameDto
import soccerstand.model._
import soccerstand.parser.{SoccerstandContentParser, SoccerstandLeagueStandingsParser}
import soccerstand.service.communication.SoccerstandCommunication
import soccerstand.util.measure.Measureable

import scala.concurrent.{ExecutionContextExecutor, Future}

class FootballEndpoint(leagueInfoRepository: LeagueInfoRepository)
                      (implicit system: ActorSystem, executor: ExecutionContextExecutor, materializer: FlowMaterializer) extends Measureable {
  
  def config = ConfigFactory.load()
  val logger = Logging(system, getClass)

  private lazy val communication = new SoccerstandCommunication(logger)

  private def fetchSoccerstandContent: Future[LiveScores] = {
    fetchSoccerstandData(communication.liveSource) { SoccerstandContentParser.parseLiveScores }
  }

  private def fetchSoccerstandLeagueStandings(country: String, leagueName: String): Future[LeagueStandings] = {
    val leagueInfo = leagueInfoRepository.findByNaturalId(country, leagueName)
    fetchSoccerstandLeagueStandings(leagueInfo)
  }

  private def fetchSoccerstandLeagueStandings(leagueInfo: LeagueInfo): Future[LeagueStandings] = {
    fetchSoccerstandData(communication.standingsSource(leagueInfo.tournamentIds)) { response =>
      SoccerstandLeagueStandingsParser.parseLeagueStandings(leagueInfo.league, response)
    }
  }

  private def fetchSoccerstandLeagueResults(leagueInfo: LeagueInfo): Future[LiveScores] = {
    fetchSoccerstandData(communication.leagueResultsSource(leagueInfo)) { leagueSoccerstandData =>
      SoccerstandContentParser.parseLiveScores(leagueSoccerstandData)
    }
  }

  private def fetchSoccerstandLatestLeagueResults(leagueInfo: LeagueInfo): Future[LatestFinishedGames] = {
    fetchSoccerstandData(communication.latestLeagueResultsSource(leagueInfo)) { latestLeagueSoccerstandData =>
      SoccerstandContentParser.parseLatestLeagueResults(latestLeagueSoccerstandData)
    }
  }

  private def fetchSoccerstandData[T](source: Source[HttpResponse])(mapResponse: String => T): Future[T] = {
    source.runWith(Sink.head).flatMap { response =>
      response.status match {
        case OK =>
          Unmarshal(response.entity).to[String].map { mapResponse }
        case _ =>
          throw new RuntimeException(s"unexpected happenned: ${response.status}")
      }
    }
  }

  val routes = {
    import soccerstand.service.protocols.JsonProtocol._
    logRequest("soccerstand-data-fetcher") {
      redirectToNoTrailingSlashIfPresent(Found) {
        pathPrefix("live") {
          (get & pathEnd){
            complete {
              ToResponseMarshallable { fetchSoccerstandContent.map { GameDto.fromLiveScores } }
            }
          } ~
          (get & pathPrefix(Segment) & pathSuffix(Segment)) { (country, leagueName) =>
            complete {
              val leagueInfo = leagueInfoRepository.findByNaturalId(country, leagueName)
              ToResponseMarshallable {
                fetchSoccerstandLeagueResults(leagueInfo).map { GameDto.fromLiveScores }
              }
            }
          }
        } ~
        pathPrefix("standings") {
          (get & pathPrefix(Segment) & path(Segment)) { (country, leagueName) =>
            complete {
              ToResponseMarshallable { fetchSoccerstandLeagueStandings(country, leagueName) }
            }
          }
        } ~
        pathPrefix("latest") {
          (get & pathPrefix(Segment) & path(Segment)) { (country, leagueName) =>
            complete {
              val leagueInfo = leagueInfoRepository.findByNaturalId(country, leagueName)
              ToResponseMarshallable {
                fetchSoccerstandLatestLeagueResults(leagueInfo).map { LatestFinishedGamesDto.toDto }
              }
            }
          }
        }
      }
    }
  }
}

object FootballEndpointService extends App {
  implicit val system = ActorSystem()
  implicit val executor = system.dispatcher
  implicit val materializer = ActorFlowMaterializer()
  val leagueInfoRepository = new LeagueInfoRepository(DBFactory.getInstance)
  val footbalEnpoint = new FootballEndpoint(leagueInfoRepository)

  Http().bind(interface = "0.0.0.0", port = 9000).startHandlingWith(footbalEnpoint.routes)
}
