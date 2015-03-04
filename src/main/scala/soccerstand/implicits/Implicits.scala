package soccerstand.implicits

import java.util.Date
import java.util.concurrent.TimeUnit

import soccerstand.parser.token.SoccerstandTokens._

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.xml.{NodeSeq, Node}

object Implicits {
  implicit class SoccerstandData(a: String) {
    val endSign = '¬'
    def readIntAt(i: Int) = a.substring(i).takeTillEndSign.toInt
    def readDateAt(i: Int) = new Date(a.substring(i).takeTillEndSign.toLong * 1000)
    def readDataAfterIdx(i: Int) = a.substring(i).takeTillEndSign
    def takeTillEndSign = a.takeWhile(_ != endSign)
    def onlyUsefulData = a.replaceAll(s"$endOfUsefulData1(.*)", "").replaceAll(s"$endOfUsefulData2(.*)", "")
  }
  implicit class SplittedString(a: String) {
    def splitOmitFirst(regex: String) = a.split(regex).tail
    def splitOmitFirst(char: Char) = a.split(char).tail
  }
  implicit class RichDate(date: Date) {
    def diffInMinutes(otherDate: Date): Int = {
      val diffInMillies = Math.abs(otherDate.getTime - date.getTime)
      TimeUnit.MINUTES.convert(diffInMillies, TimeUnit.MILLISECONDS).toInt
    }
  }
  implicit class RichString(s: String) {
    def withoutWhitespaces = s.replaceAll(" ", "")
    def normalizedLeagueName = withoutSeasonSpecifics.withoutWhitespaces
    def whitespacesToDashes = s.replaceAll(" ", "-").replaceAll("---", "-").replaceAll("'", "-")
    def withoutSeasonSpecifics = s.replaceAll(" -(.*)", "")
    def withoutParens = s.replaceAll("[()]", "")
    def withoutWhitespacesAtFrontAndBack = {
      val head = s.head.toString
      val last = s.last.toString
      (head, last) match {
        case (" ", _) => s.tail
        case (_, " ") => s.init
        case (" ", " ") => s.drop(1).dropRight(1)
        case _ => s
      }
    }
  }
  implicit class HtmlString(h: String) {
    def wrapInDiv = s"<div>$h</div>"
    def dataInsideTagRegex = s"(?i)<$h([^>]+)>(.+?)</$h>".r
    def withoutNbsp = h.replaceAll("&nbsp;", "")
  }
  
  implicit class MatchSummaryXml(xml: NodeSeq) {
    def getTextFromClass(htmlClass: String) = xml.find { n => n \@ "class" == htmlClass}.get.text
  }
  
  implicit class RichCollection[A, Repr](xs: IterableLike[A, Repr]){
    def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]) = {
      val builder = cbf(xs.repr)
      val i = xs.iterator
      var set = Set[B]()
      while (i.hasNext) {
        val o = i.next()
        val b = f(o)
        if (!set(b)) {
          set += b
          builder += o
        }
      }
      builder.result()
    }
  }
}
