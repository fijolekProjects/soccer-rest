package soccerstand.service.protocols

import java.util.Date

import soccerstand.dto.FinishedMatchesDto.{FinishedMatchDto, LatestFinishedMatchesDto, RoundMatches}
import soccerstand.dto.MatchDto
import soccerstand.model._
import soccerstand.parser.MatchLineupsParser.TeamMembers.{Coach, Player}
import soccerstand.parser.MatchLineupsParser.{TeamLineup, Lineups, MatchLineups}
import soccerstand.parser.matchstats._
import soccerstand.parser.matchsummary.model.MatchEvent._
import soccerstand.parser.matchsummary.model.{MatchEvent, MatchSummary}
import spray.json._

object JsonProtocol extends DefaultJsonProtocol with NullOptions {
  implicit val leagueFormat = jsonFormat2(League.apply)
  implicit object TeamFormat extends JsonWriteFormat[Team] {
    override def write(obj: Team): JsValue = JsObject(Map("id" -> JsString(obj.naturalId.value), "name" -> JsString(obj.name)))
  }

  implicit object TeamMatchResultFormat extends JsonWriteFormat[TeamMatchResult] {
    override def write(obj: TeamMatchResult): JsValue = {
      val goals = obj.goals match {
        case None => JsNull
        case Some(a) => JsNumber(a)
      }
      JsObject(TeamFormat.write(obj.team).asJsObject.fields + ("goals" -> goals))
    }
  }

  implicit val matchDtoFormat = jsonFormat7(MatchDto.apply)
  implicit object MatchResultStatusFormat extends CaseObjectFormat[MatchResultStatus]
  implicit val teamMatch = jsonFormat5(TeamMatch.apply)
  implicit val teamForm = jsonFormat1(TeamForm.apply)
  implicit object StandingFormat extends JsonWriteFormat[TeamStanding] {
    override def write(obj: TeamStanding): JsValue = {
      JsObject(Map(
        "id" -> JsString(obj.team.naturalId.value),
        "name" -> JsString(obj.team.name),
        "rank" -> JsNumber(obj.rank),
        "goalsScored" -> JsNumber(obj.goalsScored),
        "goalsConcealed" -> JsNumber(obj.goalsConcealed),
        "draws" -> JsNumber(obj.draws),
        "matchesPlayed" -> JsNumber(obj.matchesPlayed),
        "points" -> JsNumber(obj.points),
        "wins" -> JsNumber(obj.wins),
        "losses" -> JsNumber(obj.losses),
        "form" -> teamForm.write(obj.form)
      ))
    }
  }
  implicit val leagueStandings = jsonFormat2(LeagueStandings.apply)
  implicit val finishedMatchDto = jsonFormat4(FinishedMatchDto.apply)
  implicit val roundMatches = jsonFormat2(RoundMatches.apply)
  implicit val latestFinishedMatchesDto = jsonFormat2(LatestFinishedMatchesDto.apply)
  implicit val latestFinishedMatchesNoRound = jsonFormat4(FinishedMatchNoRound.apply)
  implicit val teamMatchesInLeague = jsonFormat2(TeamMatchesInLeague.apply)
  implicit val latestTeamFinishedMatches = jsonFormat1(LatestTeamFinishedMatches.apply)
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

  object OffMatchMissedPenaltyFormat extends MatchEventFormatWriter[OffMatchMissedPenalty] {
    override val writer = jsonFormat2(OffMatchMissedPenalty.apply)
    override val eventName: String = "missed penalty"
  }

  object ScoredPenaltyFormat extends MatchEventFormatWriter[ScoredPenalty] {
    override val writer = jsonFormat2(ScoredPenalty.apply)
    override val eventName: String = "scored penalty"
  }

  object OffMatchScoredPenaltyFormat extends MatchEventFormatWriter[OffMatchScoredPenalty] {
    override val writer = jsonFormat2(OffMatchScoredPenalty.apply)
    override val eventName: String = "scored penalty"
  }

  implicit object MatchStageEventsFormat extends JsonWriteFormat[MatchStageEvents] {
    override def write(obj: MatchStageEvents): JsValue = {
      val stageEventsMap = Map(
        "firstHalf" -> ManyMatchEventsFormat.write(obj.firstHalf.teamEvents.events),
        "secondHalf" -> ManyMatchEventsFormat.write(obj.secondHalf.teamEvents.events),
        "extraTime" -> ManyMatchEventsFormat.write(obj.extraTime.teamEvents.events),
        "penalties" -> ManyMatchEventsFormat.write(obj.penalties.teamEvents.events)
      )
      JsObject(stageEventsMap)
    }
  }

  implicit val matchEvents = jsonFormat2(MatchEvents.apply)

  implicit val matchFormat = jsonFormat6(Match.apply)
  implicit val matchSummary = jsonFormat3(MatchSummary.apply)

  implicit val matchStatName = new JsonWriteFormat[MatchStatName] {
    override def write(obj: MatchStatName): JsValue = JsString(obj.statName)
  }

  implicit val matchStat = jsonFormat3(MatchStat.apply)
  implicit val stats = jsonFormat4(Stats.apply)
  implicit val matchStatistics = jsonFormat3(MatchStatistics.apply)

  implicit val coach = jsonFormat2(Coach.apply)
  implicit val player = jsonFormat3(Player.apply)
  implicit val teamLineups = jsonFormat3(TeamLineup.apply)
  implicit val lineups = jsonFormat2(Lineups.apply)
  implicit val matchLineups = jsonFormat3(MatchLineups.apply)

  object ManyMatchEventsFormat extends JsonWriteFormat[Seq[MatchEvent]] {
    override def write(obj: Seq[MatchEvent]): JsValue = JsArray(obj.map(MatchEventFormat.write).toVector)
  }

  implicit object MatchEventFormat extends JsonWriteFormat[MatchEvent] {
    override def write(obj: MatchEvent): JsValue = obj match {
      case o: YellowCard => YellowCardFormat.write(o)
      case o: SecondYellowCard => SecondYellowCardFormat.write(o)
      case o: RedCard => RedCardFormat.write(o)
      case o: Substitution => SubstitutionFormat.write(o)
      case o: Goal => GoalFormat.write(o)
      case o: OwnGoal => OwnGoalFormat.write(o)
      case o: MissedPenalty => MissedPenaltyFormat.write(o)
      case o: ScoredPenalty => ScoredPenaltyFormat.write(o)
      case o: OffMatchMissedPenalty => OffMatchMissedPenaltyFormat.write(o)
      case o: OffMatchScoredPenalty => OffMatchScoredPenaltyFormat.write(o)
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

