package soccerstand.service

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import db.repository.{LeagueInfoRepository, TeamInfoRepository}
import soccerstand.dto.FinishedMatchesDto.LatestFinishedMatchesDto
import soccerstand.dto.MatchDto
import soccerstand.model._
import soccerstand.parser.MatchLineupsParser.MatchLineups
import soccerstand.parser.SoccerstandContentParser
import soccerstand.parser.matchstats.MatchStatistics
import soccerstand.parser.matchsummary.model.MatchSummary
import soccerstand.service.communication.{SoccerstandCommunication, SoccerstandSource}

import scala.concurrent.{ExecutionContextExecutor, Future}

class FootballEndpoint(leagueInfoRepository: LeagueInfoRepository, teamInfoRepository: TeamInfoRepository)
                      (implicit actorDeps: ActorDeps) {
  implicit val (system, executor, materializer) = actorDeps.unpack
  val logger = Logging(system, getClass)

  private lazy val communication = new SoccerstandCommunication(logger)
  def newSoccerstandContentParser = new SoccerstandContentParser(leagueInfoRepository, teamInfoRepository)

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
        } ~
        pathPrefix("team") {
          (get & pathSuffix(Segment)) { naturalTeamId =>
            val teamInfo = teamInfoRepository.findByNaturalId(NaturalTeamId(naturalTeamId))
            val teamResults = fetchSoccerstandLatestTeamResults(teamInfo)
            complete { ToResponseMarshallable(teamResults) }
          }
        } ~
        pathPrefix("stats") {
          (get & pathSuffix(Segment)) { matchId =>
            complete { ToResponseMarshallable(fetchSoccerstandMatchStatistics(matchId)) }
          }
        } ~
        pathPrefix("lineups") {
          (get & pathSuffix(Segment)) { matchId =>
            complete { ToResponseMarshallable(fetchSoccerstandMatchLineups(matchId)) }
          }
        }
      }
    }
  }
  
  private def getUserLeagueInfoAndCompleteWith[T: ToResponseMarshaller](f: LeagueInfo => T) = {
    val directive = get & pathPrefix(Segment) & pathSuffix(Segment)
    directive { (country, leagueName) =>
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

  private def fetchSoccerstandLatestTeamResults(teamInfo: TeamInfo): Future[LatestTeamFinishedMatches] = {
    communication.latestTeamResultsSource(teamInfo).fetchSoccerstandData { latestTeamResultsSource =>
      newSoccerstandContentParser.parseLatestTeamResults(latestTeamResultsSource)
    }
  }

  private def fetchSoccerstandMatchSummary(matchId: String): Future[MatchSummary] = {
    val (htmlMatchSummaryDataF, matchDetailsF, matchHtmlF) = fetchMatchDetails(matchId, communication.matchSummarySource(matchId))
    for {
      htmlMatchSummaryData <- htmlMatchSummaryDataF
      matchDetails <- matchDetailsF
      matchHtml <- matchHtmlF
    } yield newSoccerstandContentParser.parseMatchSummary(matchId, htmlMatchSummaryData, matchDetails, matchHtml)
  }

  private def fetchSoccerstandMatchStatistics(matchId: String): Future[MatchStatistics] = {
    val (matchStatsHtmlF, matchDetailsF, matchHtmlF) = fetchMatchDetails(matchId, communication.matchHtmlStatistics(matchId))
    for {
      matchStatsHtml <- matchStatsHtmlF
      matchDetails <- matchDetailsF
      matchHtml <- matchHtmlF
    } yield newSoccerstandContentParser.parseMatchStatistics(matchId, matchStatsHtml, matchDetails, matchHtml)
  }

  private def fetchSoccerstandMatchLineups(matchId: String): Future[MatchLineups] = {
    val (matchLineupsHtmlF, matchDetailsF, matchHtmlF) = fetchMatchDetails(matchId, communication.matchHtmlLineups(matchId))
    for {
      matchLineupsHtml <- matchLineupsHtmlF
      matchDetails <- matchDetailsF
      matchHtml <- matchHtmlF
    } yield newSoccerstandContentParser.parseLineups(matchId, matchLineupsHtml, matchDetails, matchHtml)
  }

  private def fetchMatchDetails(matchId: String, customSource: SoccerstandSource): (Future[String], Future[String], Future[String]) /*make it a class when frequently used*/ = {
    val customSourceDataF = customSource.fetchSoccerstandData(identity)
    val matchDetailsF = communication.matchDetailsSource(matchId).fetchSoccerstandData(identity)
    val matchHtmlF = communication.matchHtmlSource(matchId).fetchSoccerstandData(identity)
    (customSourceDataF, matchDetailsF, matchHtmlF)
  }
}

object FootballEndpointService extends App {
  implicit val actorDeps = ActorDeps.default
  implicit val (system, executor, materializer) = actorDeps.unpack
  val leagueInfoRepository = new LeagueInfoRepository()
  val teamInfoRepository = new TeamInfoRepository()
  val footbalEnpoint = new FootballEndpoint(leagueInfoRepository, teamInfoRepository)

  Http().bindAndHandle(handler = footbalEnpoint.routes, interface = "0.0.0.0", port = 9000)
}

class ActorDeps(system: ActorSystem, executor: ExecutionContextExecutor, materializer: ActorMaterializer) {
  val unpack = (system, executor, materializer)
}
object ActorDeps {
  implicit val system = ActorSystem()
  implicit val executor = system.dispatcher
  implicit val materializer = ActorMaterializer()
  def default = new ActorDeps(system, executor, materializer) 
}
