package db.repository

import com.mongodb.casbah.MongoDB
import db.DBFactory

trait DBRepository {
  protected val db: MongoDB = DBFactory.getInstance
}
