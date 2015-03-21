package soccerstand.implicits

import java.net.{URLConnection, URL}
import java.util.Date
import java.util.concurrent.TimeUnit

import soccerstand.parser.token.SoccerstandTokens._

import scala.collection.generic.CanBuildFrom
import scala.collection.{IterableLike, SeqLike}
import scala.xml.NodeSeq

object Implicits {
  //DOIT it should be value class
  implicit class SoccerstandData(a: String) {
    val endSign = '¬'
    def readIntAt(i: Int) = a.substring(i).takeTillEndSign.toInt
    def readDateAt(i: Int) = new Date(a.substring(i).takeTillEndSign.toLong * 1000)
    def readDataAfterIdx(i: Int) = a.substring(i).takeTillEndSign
    def takeTillEndSign = a.takeWhile(_ != endSign)
    def onlyUsefulData = a.replaceAll(s"$endOfUsefulData1(.*)", "").replaceAll(s"$endOfUsefulData2(.*)", "")
  }
  implicit class SplittedString(a: String) {
    def splitOmitFirst(regex: String): Array[String] = a.split(regex).tail
    def splitOmitFirst(char: Char): Array[String] = a.split(char).tail
  }
  implicit class RichDate(date: Date) {
    def diffInMinutes(otherDate: Date): Int = {
      val diffInMillies = Math.abs(otherDate.getTime - date.getTime)
      TimeUnit.MINUTES.convert(diffInMillies, TimeUnit.MILLISECONDS).toInt
    }
  }
  implicit class RichString(s: String) {
    def withoutWhitespaces = s.replaceAll(" ", "")
    def whitespacesToDashes = s.replaceAll(" ", "-").replaceAll("---", "-").replaceAll("'", "-")
    // DOIT: 2 functions below should be elsewhere
    def normalizedLeagueName = withoutSeasonSpecifics.withoutWhitespaces
    def withoutSeasonSpecifics = s.replaceAll(" -(.*)", "")
    def withoutParens = s.replaceAll("[()]", "")
    def withoutWhitespacesAtFrontAndBack = {
      val head = s.head.toString
      val last = s.last.toString
      (head, last) match {
        case (" ", " ") => s.drop(1).dropRight(1)
        case (" ", _) => s.tail
        case (_, " ") => s.init
        case _ => s
      }
    }
    def separateAt(sign: String): (String, String) = {
      val (left, right) = s.splitAt(s.indexOf(sign))
      val rightWithoutSign = right.drop(sign.length)
      (left, rightWithoutSign)
    }
    def dataAfter(sign: Char) = {
      val lastWhitespaceIndex = s.lastIndexOf(sign)
      s.drop(lastWhitespaceIndex + 1).withoutWhitespacesAtFrontAndBack
    }

  }
  implicit class HtmlString(h: String) {
    def wrapInDiv = s"<div>$h</div>"
    def dataInsideTagRegex = s"(?i)<$h.*?>(.+?)</$h>".r
    def withoutNbsp = h.replaceAll("&nbsp;", "")
  }

  //DOIT it should be value class
  implicit class MatchSummaryXml(xml: NodeSeq) {
    def getTextFromClass(htmlClass: String): String = findTextFromClass(htmlClass).get
    def findTextFromClass(htmlClass: String): Option[String] = xml.find { n => (n \@ "class").contains(htmlClass) }.map(_.text)
  }

  implicit class RichUrl(url: URL) {
    def setRequestProp(key: String, value: String): URLConnection = {
      val req = url.openConnection
      req.setRequestProperty(key, value)
      req
    }
  }

  implicit class RichUrlConnection(urlConnection: URLConnection) {
    def makeGetRequest(): String = scala.io.Source.fromInputStream(urlConnection.getInputStream).mkString
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
  implicit class StringCollection[Repr](xs: SeqLike[String, Repr]) {
    def containsElemWithWord(searchedString: String): Boolean = {
      xs.exists { elem => elem.contains(searchedString) }
    }
  }

  implicit class NestedMap[A, B, C](m: Map[A, Map[B, C]]) {
    def sequence: Map[B, Map[A, C]] = {
      //DOIT too complicated! maybe get rid of these Maps?
      val seq = for {
        (a, mapbc) <- m.toSeq
        (b, c) <- mapbc
      } yield b -> (a -> c)
      seq.groupBy { case (bb, _) => bb }.mapValues { s => s.map { case ((_, ac)) => ac }.toMap }
    }
  }

}
