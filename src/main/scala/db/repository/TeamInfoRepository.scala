package db.repository

import com.mongodb.casbah.Imports._
import db.ConvertFromDBObject
import soccerstand.model.{NaturalTeamId, TeamInfo}

class TeamInfoRepository extends DBRepository {
  private def teamInfoColl = db("teamInfo")
  
  def createOrUpdateAll(teamInfos: Iterable[TeamInfo]): Unit = {
    teamInfos.foreach { teamInfo =>
      val toSave = teamInfo.toDBObject
      teamInfoColl.update(Map("id" -> teamInfo.id), toSave, upsert = true)
    }
  }

  def findByNaturalId(naturalTeamId: NaturalTeamId): TeamInfo = {
    val where = Map("naturalId" -> Map("value" -> naturalTeamId.value))
    val teamInfo = teamInfoColl.findOne(where).get
    ConvertFromDBObject.asObject[TeamInfo](teamInfo)
  }
}
