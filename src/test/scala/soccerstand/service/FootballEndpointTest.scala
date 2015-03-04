package soccerstand.service

import akka.http.model.ContentTypes._
import akka.http.model.StatusCodes._
import akka.http.testkit.{RouteTestTimeout, ScalatestRouteTest}
import db.repository.LeagueInfoRepository
import org.mockito.Mockito.when
import org.scalatest._
import org.scalatest.mock.MockitoSugar
import soccerstand.model._

import scala.concurrent.duration._

class FootballEndpointTest extends FlatSpec with Matchers with ScalatestRouteTest with MockitoSugar {
  implicit val timeout = RouteTestTimeout(10.second)

  val bundesligaLeagueInfo = LeagueInfo(
    league = League(Country("GERMANY", 81), "Bundesliga"),
    tournamentIds = TournamentIds("M1VFOdWr", "pYi5cMuA"),
    tournamentNumIds = TournamentNumIds(160, 160)
  )
  val leagueInfoRepository = mock[LeagueInfoRepository]
  when(leagueInfoRepository.findByNaturalId("germany", "bundesliga")).thenReturn(bundesligaLeagueInfo)
  
  val footballEndpoint = new FootballEndpoint(leagueInfoRepository)

  val routes = footballEndpoint.routes
  it should "return 200 for base routes" in {
    Get(s"/today") ~> routes ~> check {
      itWorks()
    }
    Get(s"/today/germany/bundesliga") ~> routes ~> check {
      itWorks()
    }
    Get(s"/latest/germany/bundesliga") ~> routes ~> check {
      itWorks()
    }
    Get(s"/standings/germany/bundesliga") ~> routes ~> check {
      itWorks()
    }
  }

  private def itWorks(): Unit = {
    status shouldBe OK
    contentType shouldBe `application/json`
  }
}

