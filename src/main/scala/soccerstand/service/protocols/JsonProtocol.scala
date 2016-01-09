package soccerstand.service.protocols

import java.time.LocalDateTime

import soccerstand.dto.FinishedMatchesDto.{FinishedMatchDto, LatestFinishedMatchesDto, RoundMatches}
import soccerstand.dto.MatchDto
import soccerstand.model._
import soccerstand.parser.MatchCommentaryParser.{Commentary, MatchCommentary, CommentaryEvent}
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

  class MatchEventFormatWriter[T <: MatchEvent](val writer: RootJsonFormat[T]) extends JsonWriteFormat[T] {
    override def write(obj: T): JsValue =
      JsObject(Map("event" -> JsString(obj.getClass.getSimpleName)) ++ writer.write(obj).asJsObject.fields)
  }

  object YellowCardFormat extends MatchEventFormatWriter[YellowCard](jsonFormat3(YellowCard.apply))
  object SecondYellowCardFormat extends MatchEventFormatWriter[SecondYellowCard](jsonFormat3(SecondYellowCard.apply))
  object RedCardFormat extends MatchEventFormatWriter[RedCard](jsonFormat3(RedCard.apply))
  object SubstitutionFormat extends MatchEventFormatWriter[Substitution](jsonFormat3(Substitution.apply))
  object GoalFormat extends MatchEventFormatWriter[Goal](jsonFormat3(Goal.apply))
  object OwnGoalFormat extends MatchEventFormatWriter[OwnGoal](jsonFormat2(OwnGoal.apply))
  object MissedPenaltyFormat extends MatchEventFormatWriter[MissedPenalty](jsonFormat2(MissedPenalty.apply))
  object OffMatchMissedPenaltyFormat extends MatchEventFormatWriter[OffMatchMissedPenalty](jsonFormat2(OffMatchMissedPenalty.apply))
  object ScoredPenaltyFormat extends MatchEventFormatWriter[ScoredPenalty](jsonFormat2(ScoredPenalty.apply))
  object OffMatchScoredPenaltyFormat extends MatchEventFormatWriter[OffMatchScoredPenalty](jsonFormat2(OffMatchScoredPenalty.apply))

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

  implicit object CommentaryEventFormat extends CaseObjectFormat[CommentaryEvent]

  implicit val commentary = jsonFormat3(Commentary.apply)
  implicit val matchCommentary = jsonFormat3(MatchCommentary.apply)

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
    override def write(obj: T): JsValue = {
      JsString(obj.getClass.getName.drop(obj.getClass.getEnclosingClass.getName.length).init.tail) /*getting case object class name inside another object is not trivial...*/
    }
  }

  implicit object LocalDateTimeJsonFormat extends JsonWriteFormat[LocalDateTime] {
    override def write(obj: LocalDateTime): JsValue = JsString(obj.toString)
  }
  
  implicit object CountryFormat extends JsonWriteFormat[Country] {
    override def write(obj: Country): JsValue = obj.name.toJson
  }

  trait JsonWriteFormat[T] extends RootJsonFormat[T] {
    override def read(json: JsValue): T = ???
  }
}

