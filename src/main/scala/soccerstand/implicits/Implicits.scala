package soccerstand.implicits

import java.net.{URL, URLConnection}
import java.time.{Instant, LocalDateTime}
import java.util.TimeZone

import soccerstand.parser.token.SoccerstandTokens._

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable._
import scala.collection.{IterableLike, SeqLike}
import scala.xml.NodeSeq

object Implicits {
  //DOIT it should be value class
  implicit class SoccerstandData(a: String) {
    private val endSign = '¬'
    def readIntAt(i: Int) = a.substring(i).takeTillEndSign.toInt
    def readDateAt(i: Int) = a.substring(i).takeTillEndSign.toDate
    def readDataAfterIdx(i: Int) = a.substring(i).takeTillEndSign
    def takeTillEndSign = a.takeWhile(_ != endSign)
    def onlyUsefulData = a.replaceAll(s"$endOfUsefulData1(.*)", "").replaceAll(s"$endOfUsefulData2(.*)", "")
    def toDate = LocalDateTime.ofInstant(Instant.ofEpochSecond(a.toLong), TimeZone.getDefault.toZoneId)
  }
  implicit class SplittedString(a: String) {
    def splitOmitFirst(regex: String): Array[String] = a.split(regex).tail
    def splitOmitFirst(char: Char): Array[String] = a.split(char).tail
  }
  implicit class RichString(s: String) {
    def withoutNewlines = s.replaceAll("[\\t\\n\\r]", "")
    def withoutWhitespaces = s.replaceAll(" ", "")
    def whitespacesToDashes = s.replaceAll(" ", "-").replaceAll("---", "-").replaceAll("'", "-")
    // DOIT: 2 functions below should be elsewhere
    def normalizedLeagueName = withoutSeasonSpecifics.withoutWhitespaces
    def withoutSeasonSpecifics = s.replaceAll(" -(.*)", "")
    def withoutParens = s.replaceAll("[()]", "")
    def withoutPercents = s.replaceAll("%", "")
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
    def extractBetween(a: Char, b: Char) = {
      s.dropWhile(_ != a).takeWhile(_ != b).tail
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
    def findTextFromClass(htmlClass: String): Option[String] = findNodeFromClass(htmlClass).map(_.text)
    def getNodeFromClass(htmlClass: String) = findNodeFromClass(htmlClass).get
    private def findNodeFromClass(htmlClass: String) = xml.find { n => (n \@ "class").contains(htmlClass) }
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

  implicit class RichTuple[A](t: (A,A)) {
    def map[B](f: A => B) = {
      val (t1, t2) = t
      (f(t1), f(t2))
    }
  }

  implicit class RichSeqDisjunction[A,B](eithers: Seq[scalaz.\/[A,B]]) {
    def unzipBoth: (Seq[A], Seq[B]) = {
      eithers.foldLeft((List.empty[A], List.empty[B])) {  case ((lefts, rights), curr) =>
        curr match {
          case scalaz.-\/(l) => (l :: lefts, rights)
          case scalaz.\/-(r) => (lefts, r :: rights)
        }
      }
    }
  }

  implicit object LocalDateTimeOrdering extends Ordering[LocalDateTime] {
    override def compare(x: LocalDateTime, y: LocalDateTime): Int = x.compareTo(y)
  }

}
