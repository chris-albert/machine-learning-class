package io.lbert.ml

import org.scalatest.{Matchers, WordSpec}

class LinearRegressionSpec extends WordSpec with Matchers {

  "meanNormalization" should {
    "normalize features" in {
      LinearRegression.meanNormalization[Double](List(2,2,4,4)) shouldBe List(
        -.5,-.5,.5,.5
      )
    }
  }
}
