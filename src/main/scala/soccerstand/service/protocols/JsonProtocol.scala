package soccerstand.service.protocols

import java.util.Date

import soccerstand.dto.FinishedGamesDto.{FinishedGameDto, LatestFinishedGamesDto, RoundGames}
import soccerstand.dto.GameDto
import soccerstand.model._
import soccerstand.parser.matchsummary.MatchSummaryParser._
import spray.json._

object JsonProtocol extends DefaultJsonProtocol with NullOptions {
  implicit val leagueFormat = jsonFormat2(League.apply)
  implicit val clubFormat = jsonFormat2(Club.apply)
  implicit val gameDtoFormat = jsonFormat7(GameDto.apply)
  implicit val standing = jsonFormat9(ClubStanding.apply)
  implicit val leagueStandings = jsonFormat2(LeagueStandings.apply)
  implicit val finishedGameDto = jsonFormat4(FinishedGameDto.apply)
  implicit val roundGames = jsonFormat2(RoundGames.apply)
  implicit val latestFinishedGamesDto = jsonFormat2(LatestFinishedGamesDto.apply)
  implicit val playerScoresFormat = jsonFormat6(PlayerScores.apply)
  implicit val topScorers = jsonFormat2(TopScorers.apply)

  implicit val matchMinute = jsonFormat2(MatchMinute.apply)
  implicit val yellowCard = jsonFormat3(YellowCard.apply)
  implicit val secondYellowCard = jsonFormat3(SecondYellowCard.apply)
  implicit val redCard = jsonFormat3(RedCard.apply)
  implicit val substitution = jsonFormat3(Substitution.apply)
  implicit val goal = jsonFormat3(Goal.apply)
  implicit val missedPenalty = jsonFormat2(MissedPenalty.apply)

  implicit val matchSummary = jsonFormat2(MatchSummary.apply)
  implicit object MatchEventFormat extends RootJsonFormat[MatchEvent] {
    override def read(json: JsValue): MatchEvent = ???
    override def write(obj: MatchEvent): JsValue = obj match {
      case o: YellowCard => yellowCard.write(o)
      case o: SecondYellowCard => secondYellowCard.write(o)
      case o: RedCard => redCard.write(o)
      case o: Substitution => substitution.write(o)
      case o: Goal => goal.write(o)
      case o: MissedPenalty => missedPenalty.write(o)
    }
  }

  implicit object GameStatusFormat extends CaseObjectFormat[GameStatus]
  implicit object PlayerPositionFormat extends CaseObjectFormat[PlayerPosition]

  trait CaseObjectFormat[T] extends RootJsonFormat[T] {
    override def read(json: JsValue): T = ???
    override def write(obj: T): JsValue = JsString(obj.getClass.getSimpleName.init)
  }

  implicit object DateJsonFormat extends RootJsonFormat[Date] {
    override def read(json: JsValue): Date = ???
    override def write(obj: Date): JsValue = JsString(obj.toString)
  }
  
  implicit object CountryFormat extends RootJsonFormat[Country] {
    override def read(json: JsValue): Country = ???
    override def write(obj: Country): JsValue = obj.name.toJson
  }
}

