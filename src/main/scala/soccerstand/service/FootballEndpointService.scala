package soccerstand.service

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.Http
import akka.http.marshallers.sprayjson.SprayJsonSupport._
import akka.http.marshalling._
import akka.http.model.HttpResponse
import akka.http.model.StatusCodes._
import akka.http.server.Directives._
import akka.http.unmarshalling.Unmarshal
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorFlowMaterializer, FlowMaterializer}
import db.repository.LeagueInfoRepository
import soccerstand.dto.FinishedMatchesDto.LatestFinishedMatchesDto
import soccerstand.dto.MatchDto
import soccerstand.model._
import soccerstand.parser.SoccerstandContentParser
import soccerstand.parser.matchsummary.model.MatchSummary
import soccerstand.service.communication.SoccerstandCommunication

import scala.concurrent.{ExecutionContextExecutor, Future}

class FootballEndpoint(leagueInfoRepository: LeagueInfoRepository)(implicit actorDeps: ActorDeps = ActorDeps.default) {
  implicit val (system, executor, materializer) = actorDeps.unpack
  val logger = Logging(system, getClass)

  private lazy val communication = new SoccerstandCommunication(logger)
  def newSoccerstandContentParser = new SoccerstandContentParser(leagueInfoRepository)

  val routes = {
    logRequest("soccerstand-data-fetcher") {
      import soccerstand.service.protocols.JsonProtocol._
      redirectToNoTrailingSlashIfPresent(Found) {
        pathPrefix("today") {
          (get & pathEnd){
            complete {
              ToResponseMarshallable { fetchSoccerstandContent.map { MatchDto.fromTodayScores } }
            }
          } ~
            getUserLeagueInfoAndCompleteWith { leagueInfo =>
              fetchSoccerstandTodayLeagueResults(leagueInfo).map { MatchDto.fromTodayScores }
            }
        } ~
        pathPrefix("standings") {
          getUserLeagueInfoAndCompleteWith { fetchSoccerstandLeagueStandings  }
        } ~
        pathPrefix("latest") {
          getUserLeagueInfoAndCompleteWith { leagueInfo =>
            fetchSoccerstandLatestLeagueResults(leagueInfo).map { LatestFinishedMatchesDto.toDto(_).latestFirst }
          }
        } ~
        pathPrefix("topscorers") {
          getUserLeagueInfoAndCompleteWith { fetchSoccerstandTopScorers }
        } ~
        pathPrefix("summary") {
          (get & pathSuffix(Segment)) { matchId =>
            complete { ToResponseMarshallable(fetchSoccerstandMatchSummary(matchId)) }
          }
        }
      }
    }
  }
  
  private def getUserLeagueInfoAndCompleteWith[T: ToResponseMarshaller](f: LeagueInfo => T) = {
    (get & pathPrefix(Segment) & pathSuffix(Segment)) { (country, leagueName) =>
      complete {
        val leagueInfo = leagueInfoRepository.findByNaturalId(country, leagueName)
        ToResponseMarshallable { f(leagueInfo) }
      }
    }
  }

  private def fetchSoccerstandContent: Future[TodayScores] = {
    communication.todaySource.fetchSoccerstandData { newSoccerstandContentParser.parseLiveScores }
  }

  private def fetchSoccerstandLeagueStandings(leagueInfo: LeagueInfo): Future[LeagueStandings] = {
    communication.standingsSource(leagueInfo.tournamentIds).fetchSoccerstandData { response =>
      newSoccerstandContentParser.parseLeagueStandings(leagueInfo.league, response)
    }
  }

  private def fetchSoccerstandTopScorers(leagueInfo: LeagueInfo): Future[TopScorers] = {
    communication.topScorersSource(leagueInfo.tournamentIds).fetchSoccerstandData { response =>
      newSoccerstandContentParser.parseTopScorers(leagueInfo.league, response)
    }
  }

  private def fetchSoccerstandTodayLeagueResults(leagueInfo: LeagueInfo): Future[TodayScores] = {
    communication.todayLeagueResultsSource(leagueInfo).fetchSoccerstandData { leagueSoccerstandData =>
      newSoccerstandContentParser.parseLiveScores(leagueSoccerstandData)
    }
  }

  private def fetchSoccerstandLatestLeagueResults(leagueInfo: LeagueInfo): Future[LatestFinishedMatches] = {
    communication.latestLeagueResultsSource(leagueInfo).fetchSoccerstandData { latestLeagueSoccerstandData =>
      newSoccerstandContentParser.parseLatestLeagueResults(latestLeagueSoccerstandData)
    }
  }

  private def fetchSoccerstandMatchSummary(matchId: String): Future[MatchSummary] = {
    for {
      htmlMatchSummaryData <- communication.matchSummarySource(matchId).fetchSoccerstandData(identity)
      matchDetails <- communication.matchDetailsSource(matchId).fetchSoccerstandData(identity)
      matchHtml <- communication.matchHtmlSource(matchId).fetchSoccerstandData(identity)
    } yield newSoccerstandContentParser.parseMatchSummary(matchId, htmlMatchSummaryData, matchDetails, matchHtml)
  }

  implicit class SoccerstandSource(source: Source[HttpResponse]) {
    def fetchSoccerstandData[T](mapResponse: String => T): Future[T] = {
      source.runWith(Sink.head).flatMap { response =>
        response.status match {
          case OK =>
            Unmarshal(response.entity).to[String].map { mapResponse }
          case _ =>
            throw new RuntimeException(s"unexpected happenned: ${response.status}")
        }
      }
    }
  }
}

object FootballEndpointService extends App {
  implicit val actorDeps = ActorDeps.default
  implicit val (system, executor, materializer) = actorDeps.unpack
  val leagueInfoRepository = new LeagueInfoRepository()
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
