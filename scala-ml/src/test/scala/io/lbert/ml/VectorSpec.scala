package io.lbert.ml

import org.scalatest.{Matchers, WordSpec}

class VectorSpec extends WordSpec with Matchers {

  "empty" should {
    "create empty vector" in {
      Vector.empty[Int] shouldBe Matrix.empty[Int]
    }
  }

  "apply" should {
    "create vector with values" in {
      Vector.apply(Seq(1,2,3)) shouldBe Matrix(Seq(
        Seq(1),
        Seq(2),
        Seq(3)
      ))
    }
  }

  "fill" should {
    "fill vector with 1's" in {
      Vector.fill(VectorSize(Row(4)),1) shouldBe Matrix(
        Seq(
          Seq(1),
          Seq(1),
          Seq(1),
          Seq(1)
        )
      )
    }
  }

  "fill func" should {
    "fill vector with function" in {
      Vector.fillFunc(VectorSize(Row(4)),_.row.underlying) shouldBe Matrix(
        Seq(
          Seq(1),
          Seq(2),
          Seq(3),
          Seq(4)
        )
      )
    }
  }
}
