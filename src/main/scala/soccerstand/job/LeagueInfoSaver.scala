package soccerstand.job

import db.{DBFactory, LeagueInfoRepository}
import soccerstand.indexes.LeagueInfoIndexes
import soccerstand.model.{League, LeagueInfo, TournamentIds, TournamentNumIds}
import soccerstand.parser.SoccerstandDataParser

import scala.util.Try

object LeagueInfoSaver {
  import soccerstand.implicits.Implicits._
  import soccerstand.parser.token.SoccerstandTokens._

  val leagueInfoRepository = new LeagueInfoRepository(DBFactory.getInstance)

  //DOIT distinguish between different kinds of soccerstandData
  def saveLeagueInfos(soccerstandData: String): Unit = {
    val leagueInfos = parseSoccerstandLeagueInfo(soccerstandData)
    leagueInfoRepository.createOrUpdateAll(leagueInfos)
    println(leagueInfos.mkString("\n"))
    println("Imported leagues count: " + leagueInfos.size)
  }

  private def parseSoccerstandLeagueInfo(soccerstandData: String): Seq[LeagueInfo] = {
    val inputSplittedByLeague = soccerstandData.onlyUsefulData.splitOmitFirst(newLeague)
    inputSplittedByLeague.par.flatMap { splittedByLeague =>
      val splittedByGames = splittedByLeague.split(newGame)
      val leagueToParse = splittedByGames.head
      parseLeagueInfo(leagueToParse)
    }.seq.toSeq
  }

  private def parseLeagueInfo(leagueToParse: String): Option[LeagueInfo] = {
    val league = League.fromString(leagueToParse)
    val tournamentNumIdsForLeague = findTournamentNumIdsForLeague(league)
    tournamentNumIdsForLeague.map { tournamentNumIds =>
      val tournamentIds = parseTournamentIds(leagueToParse)
      LeagueInfo(league, tournamentIds, tournamentNumIds)
    }
  }

  private def findTournamentNumIdsForLeague(league: League): Option[TournamentNumIds] = {
    val urlPart = league.soccerstandResultsUrlPart
    val soccerstandResultsHtmlDataForLeague = Try {
      scala.io.Source.fromURL(s"http://www.soccerstand.com/soccer/$urlPart/results/").mkString
    }.toOption
    soccerstandResultsHtmlDataForLeague.map(tournamentNumIdsForLeague)
  }

  private def tournamentNumIdsForLeague(soccerstandResultsHtmlData: String): TournamentNumIds = {
    val tournamentPageSeasonResultsPat = """<div id="tournament-page-season-results">(.+?)</div>""".r
    val tournamentIdPat = "tournament_id = '(.+?)';".r
    val tournamentIdNum = tournamentIdPat.findFirstMatchIn(soccerstandResultsHtmlData).get.group(1).toInt
    val tournamentPageSeasonResults = tournamentPageSeasonResultsPat.findFirstMatchIn(soccerstandResultsHtmlData).get.group(1).toInt
    TournamentNumIds(tournamentIdNum, tournamentPageSeasonResults)
  }

  private def parseTournamentIds(leagueToParse: String): TournamentIds = {
    SoccerstandDataParser.parse(leagueToParse)(LeagueInfoIndexes) { leagueInfoIndexes =>
      TournamentIds.fromString(leagueToParse, leagueInfoIndexes)
    }
  }
}