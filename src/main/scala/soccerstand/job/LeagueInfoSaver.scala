package soccerstand.job

import java.net.URL

import db.repository.LeagueInfoRepository
import soccerstand.indexes.TournamentIdsIndexes
import soccerstand.model.{League, LeagueInfo, TournamentIds, TournamentNumIds}
import soccerstand.parser.SoccerstandDataParser
import soccerstand.service.communication.SoccerstandCommunication._
import soccerstand.util.{Measureable, Slf4jLogging}

import scala.util.Try

object LeagueInfoSaver extends Slf4jLogging with Measureable {
  import soccerstand.implicits.Implicits._
  import soccerstand.parser.token.SoccerstandTokens._

  val leagueInfoRepository = new LeagueInfoRepository()

  def main(args: Array[String]) {
    val basicInfoForAllLeagues = measure("fetching info for leagues from all countries") {
      val soccerstandIndexHtml = scala.io.Source.fromURL("http://www.soccerstand.com").mkString
      val countryIdPattern = s"""<li id="lmenu_($anyContent)">""".r
      val idsForAllCountries = countryIdPattern.findAllMatchIn(soccerstandIndexHtml).toList.map { _.group(1).toInt }
      assert(idsForAllCountries.nonEmpty, "no country ids was found!")
      info(s"countries to fetch data for: ${idsForAllCountries.size}")
      idsForAllCountries.par.flatMap { collectDataForAllLeaguesWithinCountry }
    }.seq
    val leaguesWithoutDuplicates = basicInfoForAllLeagues.distinctBy { league => league.naturalId + league.leagueName}
    measure(s"saving all base league data for ${leaguesWithoutDuplicates.size} leagues") {
      leagueInfoRepository.createOrUpdateAll(leaguesWithoutDuplicates)
    }
  }

  private def collectDataForAllLeaguesWithinCountry(countryId: Int): Seq[LeagueInfo] = {
    val allLeaguesWithinCountryUrl = s"http://$soccerstandBackendRoute/x/feed/c_1_${countryId}_1_en_y_1"
    val matchesForAllLeaguesWithinCountry = new URL(allLeaguesWithinCountryUrl).setRequestProp("X-Fsign", "SW9D1eZo").makeGetRequest()
    infoBlock(s"collecting data for $countryId") {
      parseSoccerstandLeagueInfo(matchesForAllLeaguesWithinCountry)
    }
  }

  private def parseSoccerstandLeagueInfo(soccerstandData: String): Seq[LeagueInfo] = {
    val inputSplittedByLeague = soccerstandData.onlyUsefulData.splitOmitFirst(newLeague)
    inputSplittedByLeague.par.flatMap { splittedByLeague =>
      val splittedByMatches = splittedByLeague.split(newMatch)
      val leagueToParse = splittedByMatches.head
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
      scala.io.Source.fromURL(s"http://$soccerstandFrontendRoute/soccer/$urlPart/results/").mkString
    }.toOption
    soccerstandResultsHtmlDataForLeague.map(tournamentNumIdsForLeague)
  }

  private def tournamentNumIdsForLeague(soccerstandResultsHtmlData: String): TournamentNumIds = {
    val tournamentPageSeasonResultsPat = s"""<div id="tournament-page-season-results">($anyContent)</div>""".r
    val tournamentIdPat = s"tournament_id = '($anyContent)';".r
    val tournamentIdNum = tournamentIdPat.findFirstMatchIn(soccerstandResultsHtmlData).get.group(1).toInt
    val tournamentPageSeasonResults = tournamentPageSeasonResultsPat.findFirstMatchIn(soccerstandResultsHtmlData).get.group(1).toInt
    TournamentNumIds(tournamentIdNum, tournamentPageSeasonResults)
  }

  private def parseTournamentIds(leagueToParse: String): TournamentIds = {
    SoccerstandDataParser.parse(leagueToParse)(TournamentIdsIndexes) { leagueInfoIndexes =>
      TournamentIds.fromString(leagueToParse, leagueInfoIndexes)
    }
  }
}