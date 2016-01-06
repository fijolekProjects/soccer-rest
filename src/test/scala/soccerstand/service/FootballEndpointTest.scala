package soccerstand.service

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.http.scaladsl.unmarshalling.Unmarshal
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
    tournamentIds = TournamentIds("Qyo0OIgA", "zcgMPDzF"),
    tournamentNumIds = TournamentNumIds("W6BOzpK2", 165)
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
      val hannoverVsBayernMatchId = "GjmZf5Xd" /*fixme make it work with matches from previous season like GbFxpC8G*/
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
      val hannoverVsBayernMatchId = "GjmZf5Xd" /*fixme make it work with matches from previous season like GbFxpC8G*/
      Get(s"/stats/$hannoverVsBayernMatchId") ~> routes ~> check {
        itWorks()
      }
    }
    scenario("return 200 for match lineups") {
      val hannoverVsBayernMatchId = "GjmZf5Xd" /*fixme make it work with matches from previous season like GbFxpC8G*/
      Get(s"/lineups/$hannoverVsBayernMatchId") ~> routes ~> check {
        itWorks()
      }
    }
    scenario("return 200 for match commentary") {
      val hannoverVsBayernMatchId = "GjmZf5Xd" /*fixme make it work with matches from previous season like GbFxpC8G*/
      Get(s"/commentary/$hannoverVsBayernMatchId") ~> routes ~> check {
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

