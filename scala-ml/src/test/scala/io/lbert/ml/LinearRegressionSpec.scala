package io.lbert.ml

import org.scalatest.{Matchers, WordSpec}
import io.lbert.ml.implicits.MatrixSyntax._

class LinearRegressionSpec extends WordSpec with Matchers {

  "meanNormalization" should {
    "normalize features" in {
      LinearRegression.meanNormalization[Double](List(2,2,4,4)) shouldBe List(
        -.5,-.5,.5,.5
      )
    }
  }

  "standardDeviation" should {
    "get standard deviation" in {
      LinearRegression.standardDeviation(List(2,4,4,4,5,5,7,9))
    }
  }

  "featureNormalization" should {
    "normalize features" in {
      LinearRegression.featureNormalization(Matrix[Double](
        Seq(
          Seq(2,4,4,4,5,5,7,9),
          Seq(2,4,4,4,5,5,7,9)
        )
      ).transpose)
    }
  }

  "cost" should {
    "get correct cost" in {
      CSV.readMatrix("profitByPopulation.csv").map{ profitMatrix =>
        val cost = for {
          features  <- Matrix.getColumn(profitMatrix,0)
          results   <- Matrix.getColumn(profitMatrix,1)
          fWithOnes  = Matrix.addFirstColumn(features, 1.0)
          theta      = Matrix.fill[Double](MatrixSize(Row(2), Column(1)), 0)
          out       <- LinearRegression.cost(fWithOnes, results)(theta)
        } yield {
          out
        }

        cost.map(roundToString) shouldBe Right(roundToString(32.0727))

      }.run
    }
  }

  def roundToString(d: Double): String = "%.4f".format(d)
}
