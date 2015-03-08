package db.repository

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoDB
import db.{DBFactory, ConvertFromDBObject}
import soccerstand.model.{TournamentIds, LeagueInfo}
import soccerstand.util.Slf4jLogging

class LeagueInfoRepository() extends DBRepository with Slf4jLogging {
  private def leagueInfoColl = db("leagueInfo")
  def createOrUpdateAll(infos: Seq[LeagueInfo]): Unit = {
    val leagueInfosToSave = prepareLeagueInfosToSave(infos)
    leagueInfosToSave.foreach { leagueInfoToSave =>
      val naturalId = leagueInfoToSave.info.naturalId
      val where = Map("naturalId" -> naturalId, "league" -> Map("leagueName" -> leagueInfoToSave.info.leagueName))
      leagueInfoColl.update(where, leagueInfoToSave.infoToSave, upsert = true)
    }
  }

  private def prepareLeagueInfosToSave(infos: Seq[LeagueInfo]): Seq[LeagueInfoToSave] = {
    val infosGroupedByNaturalId = infos.groupBy { _.naturalId }
    val infosToSave = for {
      (_, leagueInfos) <- infosGroupedByNaturalId
      (info, priority) <- leagueInfos.zipWithIndex
    } yield LeagueInfoToSave(info, info.toDBObject ++ ("priority" -> priority, "naturalId" -> info.naturalId))
    infosToSave.toSeq.sortBy(_.info.countryName)
  }

  def findByNaturalId(countryName: String, leagueName: String): LeagueInfo = {
    val naturalId = LeagueInfo.createNaturalId(countryName, leagueName)
    //DOIT handle when naturalId is not in DB
    val infoFromDB = leagueInfoColl.findOne(Map("naturalId" -> naturalId, "priority" -> 0)).get
    ConvertFromDBObject.asObject[LeagueInfo](infoFromDB)
  }

  def findByTournamentIds(tournamentIds: TournamentIds): LeagueInfo = {
    val tournamentIdsQuery = Map(
      "tournamentIdString" -> tournamentIds.tournamentIdString, "tournamentStageId" -> tournamentIds.tournamentStageId
    )
    val where = Map("tournamentIds" -> tournamentIdsQuery)
    val infoFromDB = leagueInfoColl.findOne(where).get
    ConvertFromDBObject.asObject[LeagueInfo](infoFromDB)
  }
  
  private case class LeagueInfoToSave(info: LeagueInfo, infoToSave: DBObject)
}

trait DBRepository {
  protected val db: MongoDB = DBFactory.getInstance
}