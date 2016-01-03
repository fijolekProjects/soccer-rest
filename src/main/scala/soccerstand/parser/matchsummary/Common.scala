package soccerstand.parser.matchsummary

object Common {
  sealed trait MatchTeamTag
  object MatchTeamTag {
    case object HomeTeam extends MatchTeamTag
    case object AwayTeam extends MatchTeamTag
  }
}
