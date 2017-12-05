package io.lbert.ml

import java.io.ByteArrayInputStream
import org.scalatest.{Matchers, WordSpec}
import scala.io.BufferedSource

class CSVSpec extends WordSpec with Matchers {

  "parse" should {
    "parse empty file" in {
      CSV.parse(createBufferedSource("")) shouldBe List.empty[List[String]]
    }
    "parse one liner" in {
      CSV.parse(createBufferedSource("a,b,1,2")) shouldBe List(List("a","b","1","2"))
    }
    "parse many liner" in {
      CSV.parse(createBufferedSource("a,b,1,2\n3,4,c,d")) shouldBe List(
        List("a","b","1","2"),
        List("3","4","c","d")
      )
    }
  }

  "read" should {
    "do nothing if IO not evaluated" in {
      CSV.read("asdf")
    }
//    "fail if file doesnt exist" in {
//      CSV.read("asdf").run
//    }
    "read test file" in {
      CSV.read("test.csv").run shouldBe List(
        List("1", "2", "3", "4"),
        List("5", "6", "7", "8"),
        List("9", "10", "11", "12")
      )
    }
  }

  "readDouble" should {
    "successfully parse file" in {
      CSV.readDouble("test.csv").run shouldBe List(
        List[Double](1, 2, 3, 4),
        List[Double](5, 6, 7, 8),
        List[Double](9, 10, 11, 12)
      )
    }
  }

  def createBufferedSource(s: String): BufferedSource =
    new BufferedSource(new ByteArrayInputStream(s.getBytes()))
}
