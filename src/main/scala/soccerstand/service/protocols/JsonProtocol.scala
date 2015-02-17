package soccerstand.service.protocols

import java.util.Date

import soccerstand.dto.FinishedGamesDto.{FinishedGameDto, LatestFinishedGamesDto, RoundGames}
import soccerstand.dto.GameDto
import soccerstand.model._
import spray.json._

object JsonProtocol extends DefaultJsonProtocol with NullOptions {
  implicit object CountryFormat extends RootJsonFormat[Country] {
    override def read(json: JsValue): Country = ???
    override def write(obj: Country): JsValue = obj.name.toJson
  }
  implicit val leagueFormat = jsonFormat2(League.apply)
  implicit val clubFormat = jsonFormat2(Club.apply)
  implicit val gameDtoFormat = jsonFormat6(GameDto.apply)
  implicit val standing = jsonFormat9(ClubStanding.apply)
  implicit val leagueStandings = jsonFormat2(LeagueStandings.apply)
  implicit val finishedGameDto = jsonFormat3(FinishedGameDto.apply)
  implicit val roundGames = jsonFormat2(RoundGames.apply)
  implicit val latestFinishedGamesDto = jsonFormat2(LatestFinishedGamesDto.apply)

  implicit object GameStatusFormat extends RootJsonFormat[GameStatus] {
    override def read(json: JsValue): GameStatus = ???
    override def write(obj: GameStatus): JsValue = JsString(obj.getClass.getSimpleName.init)
  }
  implicit object DateJsonFormat extends RootJsonFormat[Date] {
    override def read(json: JsValue): Date = ???
    override def write(obj: Date): JsValue = {
      JsString(obj.toString)
    }
  }
}

