package db

import com.mongodb.casbah.Imports._
import com.novus.salat._
import com.novus.salat.global._

trait ConvertableToDBObject {
  def toDBObject: DBObject = grater[this.type].asDBObject(this)
}

object ConvertFromDBObject {
  def asObject[T <: scala.AnyRef: Manifest](dbo: DBObject) = grater[T].asObject(dbo)
}