package soccerstand.service.protocols

import java.util.Date

import soccerstand.dto.FinishedGamesDto.{FinishedGameDto, LatestFinishedGamesDto, RoundGames}
import soccerstand.dto.GameDto
import soccerstand.model._
import soccerstand.parser.matchsummary.model.MatchEvent
import soccerstand.parser.matchsummary.model.MatchEvent._
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

  implicit object MatchMinuteFormat extends JsonWriteFormat[MatchMinute] {
    override def write(obj: MatchMinute): JsValue = JsString(obj.prettyPrint)
  }

  trait MatchEventFormatWriter[T <: MatchEvent] extends JsonWriteFormat[T] {
    val writer: RootJsonFormat[T]
    val eventName: String
    override def write(obj: T): JsValue =
      JsObject(Map("event" -> JsString(eventName)) ++ writer.write(obj).asJsObject.fields)
  }

  object YellowCardFormat extends MatchEventFormatWriter[YellowCard] {
    override val writer = jsonFormat3(YellowCard.apply)
    override val eventName: String = "yellow card"
  }

  object SecondYellowCardFormat extends MatchEventFormatWriter[SecondYellowCard] {
    override val writer = jsonFormat3(SecondYellowCard.apply)
    override val eventName: String = "second yellow card"
  }

  object RedCardFormat extends MatchEventFormatWriter[RedCard] {
    override val writer = jsonFormat3(RedCard.apply)
    override val eventName: String = "red card"
  }

  object SubstitutionFormat extends MatchEventFormatWriter[Substitution] {
    override val writer = jsonFormat3(Substitution.apply)
    override val eventName: String = "substitution"
  }

  object GoalFormat extends MatchEventFormatWriter[Goal] {
    override val writer = jsonFormat3(Goal.apply)
    override val eventName: String = "goal"
  }

  object MissedPenaltyFormat extends MatchEventFormatWriter[MissedPenalty] {
    override val writer = jsonFormat2(MissedPenalty.apply)
    override val eventName: String = "missed penalty"
  }

  object ScoredPenaltyFormat extends MatchEventFormatWriter[ScoredPenalty] {
    override val writer = jsonFormat2(ScoredPenalty.apply)
    override val eventName: String = "scored penalty"
  }

  implicit val matchSummary = jsonFormat2(MatchSummary.apply)
  implicit object MatchEventFormat extends RootJsonFormat[MatchEvent] {
    override def read(json: JsValue): MatchEvent = ???
    override def write(obj: MatchEvent): JsValue = obj match {
      case o: YellowCard => YellowCardFormat.write(o)
      case o: SecondYellowCard => SecondYellowCardFormat.write(o)
      case o: RedCard => RedCardFormat.write(o)
      case o: Substitution => SubstitutionFormat.write(o)
      case o: Goal => GoalFormat.write(o)
      case o: MissedPenalty => MissedPenaltyFormat.write(o)
      case o: ScoredPenalty => ScoredPenaltyFormat.write(o)
    }
  }

  implicit object GameStatusFormat extends CaseObjectFormat[GameStatus]
  implicit object PlayerPositionFormat extends CaseObjectFormat[PlayerPosition]

  trait CaseObjectFormat[T] extends JsonWriteFormat[T] {
    override def write(obj: T): JsValue = JsString(obj.getClass.getSimpleName.init)
  }

  implicit object DateJsonFormat extends JsonWriteFormat[Date] {
    override def write(obj: Date): JsValue = JsString(obj.toString)
  }
  
  implicit object CountryFormat extends JsonWriteFormat[Country] {
    override def write(obj: Country): JsValue = obj.name.toJson
  }

  trait JsonWriteFormat[T] extends RootJsonFormat[T] {
    override def read(json: JsValue): T = ???
  }
}

