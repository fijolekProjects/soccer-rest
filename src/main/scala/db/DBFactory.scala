package db

import com.mongodb.casbah.{MongoClient, MongoDB}

object DBFactory {
  val getInstance: MongoDB = {
    val mongoClient = MongoClient("localhost", 27017)
    mongoClient("soccer-api")
  }
}