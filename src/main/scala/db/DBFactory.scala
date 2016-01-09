package db

import com.mongodb.casbah.{MongoClient, MongoDB}

object DBFactory {
  val getInstance: MongoDB = {
    val mongoClient = MongoClient("db", 27017)
    mongoClient("soccer-rest")
  }
}