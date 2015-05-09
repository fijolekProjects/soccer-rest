package soccerstand.service

import akka.http.model.ContentTypes._
import akka.http.model.StatusCodes._
import akka.http.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.http.unmarshalling.Unmarshal
import db.repository.{TeamInfoRepository, LeagueInfoRepository}
import org.mockito.Mockito.when
import org.scalatest._
import org.scalatest.mock.MockitoSugar
import soccerstand.model._

import scala.concurrent.duration._

class FootballEndpointTest extends FeatureSpec with Matchers with ScalatestRouteTest with MockitoSugar {
  implicit val timeout = RouteTestTimeout(10.second)

  val bundesliga = League(Country("GERMANY", 81), "Bundesliga")
  val bundesligaLeagueInfo = LeagueInfo(
    league = bundesliga,
    tournamentIds = TournamentIds("M1VFOdWr", "pYi5cMuA"),
    tournamentNumIds = TournamentNumIds(160, 160)
  )

  val bayernMunich = TeamInfo(
    id = SoccerstandTeamId("nVp0wiqd"),
    naturalId = NaturalTeamId("BayernMunichgermanybundesliga"),
    name = "Bayern Munich",
    league = bundesliga
  )

  val leagueInfoRepository = mock[LeagueInfoRepository]
  val teamInfoRepository = mock[TeamInfoRepository]
  when(leagueInfoRepository.findByNaturalId("germany", "bundesliga")).thenReturn(bundesligaLeagueInfo)
  when(leagueInfoRepository.findByTournamentIds(bundesligaLeagueInfo.tournamentIds)).thenReturn(bundesligaLeagueInfo)
  when(teamInfoRepository.findByNaturalId(bayernMunich.naturalId)).thenReturn(bayernMunich)

  val footballEndpoint = new FootballEndpoint(leagueInfoRepository, teamInfoRepository)(ActorDeps.default)

  val routes = footballEndpoint.routes

  feature("service should return 200 for basic routes") {
    scenario("today route") {
      Get(s"/today") ~> routes ~> check {
        itWorks()
      }
    }

    scenario("return 200 for today league route") {
      Get(s"/today/germany/bundesliga") ~> routes ~> check {
        itWorks()
      }
    }

    scenario("return 200 for today latest route") {
      Get(s"/latest/germany/bundesliga") ~> routes ~> check {
        itWorks()
      }
    }

    scenario("return 200 for standings route") {
      Get(s"/standings/germany/bundesliga") ~> routes ~> check {
        itWorks()
      }
    }

    scenario("return 200 for topscorers route") {
      Get(s"/topscorers/germany/bundesliga") ~> routes ~> check {
        itWorks()
      }
    }
    scenario("return 200 for summary match route") {
      val hannoverVsBayernMatchId = "GbFxpC8G"
      Get(s"/summary/$hannoverVsBayernMatchId") ~> routes ~> check {
        itWorks()
      }
    }
    scenario("return 200 for team matches route") {
      Get(s"/team/${bayernMunich.naturalId.value}") ~> routes ~> check {
        itWorks()
      }
    }
    scenario("return 200 for match statistics") {
      val hannoverVsBayernMatchId = "GbFxpC8G"
      Get(s"/stats/$hannoverVsBayernMatchId") ~> routes ~> check {
        itWorks()
      }
    }

  }

  private def itWorks(): Unit = {
    val responseF = Unmarshal(response.entity).to[String]
    responseF.foreach(println)
    status shouldBe OK
    contentType shouldBe `application/json`
  }
}

