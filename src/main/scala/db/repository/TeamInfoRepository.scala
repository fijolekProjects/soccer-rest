package db.repository

import com.mongodb.casbah.Imports._
import soccerstand.model.TeamInfo

class TeamInfoRepository extends DBRepository {
  private def teamInfoColl = db("teamInfo")
  
  def createOrUpdateAll(teamInfos: Iterable[TeamInfo]): Unit = {
    teamInfos.foreach { teamInfo =>
      val toSave = teamInfo.toDBObject ++ ("fakeId" -> teamInfo.naturalId)
      teamInfoColl.update(Map("id" -> teamInfo.id), toSave, upsert = true)
    }
  }
}
