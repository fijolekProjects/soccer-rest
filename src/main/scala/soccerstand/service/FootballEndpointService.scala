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
import db.DBFactory
import db.repository.LeagueInfoRepository
import soccerstand.dto.FinishedGamesDto.LatestFinishedGamesDto
import soccerstand.dto.GameDto
import soccerstand.model._
import soccerstand.parser.SoccerstandContentParser
import soccerstand.service.communication.SoccerstandCommunication

import scala.concurrent.{ExecutionContextExecutor, Future}

class FootballEndpoint(leagueInfoRepository: LeagueInfoRepository)(implicit actorDeps: ActorDeps = ActorDeps.default) {
  implicit val (system, executor, materializer) = actorDeps.unpack
  val logger = Logging(system, getClass)

  private lazy val communication = new SoccerstandCommunication(logger)

  private def fetchSoccerstandContent: Future[TodayScores] = {
    fetchSoccerstandData(communication.todaySource) { SoccerstandContentParser.parseLiveScores }
  }

  private def fetchSoccerstandLeagueStandings(country: String, leagueName: String): Future[LeagueStandings] = {
    val leagueInfo = leagueInfoRepository.findByNaturalId(country, leagueName)
    fetchSoccerstandLeagueStandings(leagueInfo)
  }

  private def fetchSoccerstandLeagueStandings(leagueInfo: LeagueInfo): Future[LeagueStandings] = {
    fetchSoccerstandData(communication.standingsSource(leagueInfo.tournamentIds)) { response =>
      SoccerstandContentParser.parseLeagueStandings(leagueInfo.league, response)
    }
  }

  private def fetchSoccerstandTopScorers(leagueInfo: LeagueInfo): Future[TopScorers] = {
    fetchSoccerstandData(communication.topScorersSource(leagueInfo.tournamentIds)) { response =>
      SoccerstandContentParser.parseTopScorers(leagueInfo.league, response)
    }
  }

  private def fetchSoccerstandTodayLeagueResults(leagueInfo: LeagueInfo): Future[TodayScores] = {
    fetchSoccerstandData(communication.todayLeagueResultsSource(leagueInfo)) { leagueSoccerstandData =>
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
        pathPrefix("today") {
          (get & pathEnd){
            complete {
              ToResponseMarshallable { fetchSoccerstandContent.map { GameDto.fromTodayScores } }
            }
          } ~
          (get & pathPrefix(Segment) & pathSuffix(Segment)) { (country, leagueName) =>
            complete {
              val leagueInfo = leagueInfoRepository.findByNaturalId(country, leagueName)
              ToResponseMarshallable {
                fetchSoccerstandTodayLeagueResults(leagueInfo).map { GameDto.fromTodayScores }
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
                fetchSoccerstandLatestLeagueResults(leagueInfo).map { LatestFinishedGamesDto.toDto(_).sortByDateLatestFirst }
              }
            }
          }
        } ~
        pathPrefix("topscorers") {
          (get & pathPrefix(Segment) & path(Segment)) { (country, leagueName) =>
            complete {
              val leagueInfo = leagueInfoRepository.findByNaturalId(country, leagueName)
              ToResponseMarshallable { fetchSoccerstandTopScorers(leagueInfo) }
            }
          }
        }
      }
    }
  }
}

object FootballEndpointService extends App {
  implicit val actorDeps = ActorDeps.default
  implicit val (system, executor, materializer) = actorDeps.unpack
  val leagueInfoRepository = new LeagueInfoRepository(DBFactory.getInstance)
  val footbalEnpoint = new FootballEndpoint(leagueInfoRepository)

  Http().bind(interface = "0.0.0.0", port = 9000).startHandlingWith(footbalEnpoint.routes)
}

class ActorDeps(system: ActorSystem, executor: ExecutionContextExecutor, materializer: FlowMaterializer) {
  val unpack = (system, executor, materializer)
}
object ActorDeps {
  implicit val system = ActorSystem()
  implicit val executor = system.dispatcher
  implicit val materializer = ActorFlowMaterializer()
  def default = new ActorDeps(system, executor, materializer) 
}
