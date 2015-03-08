package soccerstand.service.protocols

import java.util.Date

import soccerstand.dto.FinishedMatchesDto.{LatestFinishedMatchesDto, FinishedMatchDto, LatestFinishedMatchesDto$, RoundMatches}
import soccerstand.dto.MatchDto
import soccerstand.model._
import soccerstand.parser.matchsummary.model.MatchEvent._
import soccerstand.parser.matchsummary.model.{MatchEvent, MatchSummary}
import spray.json._

object JsonProtocol extends DefaultJsonProtocol with NullOptions {
  implicit val leagueFormat = jsonFormat2(League.apply)
  implicit val teamFormat = jsonFormat2(Team.apply)
  implicit val matchDtoFormat = jsonFormat7(MatchDto.apply)
  implicit val standing = jsonFormat9(TeamStanding.apply)
  implicit val leagueStandings = jsonFormat2(LeagueStandings.apply)
  implicit val finishedMatchDto = jsonFormat4(FinishedMatchDto.apply)
  implicit val roundMatches = jsonFormat2(RoundMatches.apply)
  implicit val latestFinishedMatchesDto = jsonFormat2(LatestFinishedMatchesDto.apply)
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

  object OwnGoalFormat extends MatchEventFormatWriter[OwnGoal] {
    override val writer = jsonFormat2(OwnGoal.apply)
    override val eventName: String = "own goal"
  }

  object MissedPenaltyFormat extends MatchEventFormatWriter[MissedPenalty] {
    override val writer = jsonFormat2(MissedPenalty.apply)
    override val eventName: String = "missed penalty"
  }

  object ScoredPenaltyFormat extends MatchEventFormatWriter[ScoredPenalty] {
    override val writer = jsonFormat2(ScoredPenalty.apply)
    override val eventName: String = "scored penalty"
  }

  implicit val matchEvents = jsonFormat2(MatchEvents.apply)
  implicit val matchFormat = jsonFormat6(Match.apply)
  implicit val matchSummary = jsonFormat3(MatchSummary.apply)

  implicit object MatchEventFormat extends RootJsonFormat[MatchEvent] {
    override def read(json: JsValue): MatchEvent = ???
    override def write(obj: MatchEvent): JsValue = obj match {
      case o: YellowCard => YellowCardFormat.write(o)
      case o: SecondYellowCard => SecondYellowCardFormat.write(o)
      case o: RedCard => RedCardFormat.write(o)
      case o: Substitution => SubstitutionFormat.write(o)
      case o: Goal => GoalFormat.write(o)
      case o: OwnGoal => OwnGoalFormat.write(o)
      case o: MissedPenalty => MissedPenaltyFormat.write(o)
      case o: ScoredPenalty => ScoredPenaltyFormat.write(o)
    }
  }

  implicit object MatchStatusFormat extends CaseObjectFormat[MatchStatus]
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

