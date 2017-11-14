package io.lbert.ml

import scala.Fractional.Implicits._

object LinearRegression {

  def gradientDecent = ???
  def normalEquation = ???

  def meanNormalization[A: Fractional](features: List[A]): List[A] = {
    val size = features.foldLeft(implicitly[Fractional[A]].zero) {
      case (i,_) => i + implicitly[Fractional[A]].one
    }
    val average = features.sum / size
    val max = features.max
    val min = features.min
    val maxMinusMin = max - min
    features.map(d => (d - average) / maxMinusMin)
  }

}
