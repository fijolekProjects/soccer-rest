package db

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.{MongoClient, MongoDB}
import soccerstand.model.LeagueInfo

object DBFactory {
  val getInstance: MongoDB = {
    val mongoClient = MongoClient("localhost", 27017)
    mongoClient("soccer-api")
  }
}

class LeagueInfoRepository(private val db: MongoDB) {
  private val leagueInfoColl = db("leagueInfo")
  def createOrUpdateAll(infos: Seq[LeagueInfo]): Unit = {
    val infosToSave = infos.map(_.toDBObject)
    infos.zip(infosToSave).foreach { case (info, infoToSave) =>
      val infoWithNaturalId = infoToSave ++ ("naturalId" -> info.naturalId)
      leagueInfoColl.update(Map("naturalId" -> info.naturalId), infoWithNaturalId, upsert = true)
    }
  }

  def findByNaturalId(countryName: String, leagueName: String): LeagueInfo = {
    val naturalId = LeagueInfo.createNaturalId(countryName, leagueName)
    //DOIT handle when naturalId is not in DB
    val infoFromDB = leagueInfoColl.findOne(Map("naturalId" -> naturalId)).get
    ConvertFromDBObject.asObject[LeagueInfo](infoFromDB)
  }
}
