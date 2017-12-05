package io.lbert.ml

import io.lbert.ml.Matrix.{MatrixResult, ValueResult}
import io.lbert.ml.fp.Monad
import scala.Fractional.Implicits._
import io.lbert.ml.implicits.MatrixSyntax._
import scala.util.{Success, Try}
import io.lbert.ml.fp.MonadInstances.tryMonad

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

  def cost2(features: Matrix[Double], results: Matrix[Double], theta: Matrix[Double]): ValueResult[Double] = {
    val f = Monad[Try].pure(features)
    val t = Monad[Try].pure(theta)
//    val a = Monad[Try].map2(f, t)((a,b) => a * b)
    val a = Monad[Try].map2(f, t)(_ * _)
//    val b = Monad[Try].map2(a,Monad[Try].pure(results))(_ - _)
    ???
  }

  def cost(features: Matrix[Double], results: Matrix[Double])(theta: Matrix[Double]): ValueResult[Double] = {
    val size = Matrix.size(features).rows.underlying
    for {
      xTheta <- features * theta
      minusY <- xTheta - results
    } yield {
      val tmp = minusY.map(Matrix.scalarPower(_, 2)).sum
      (1.0 / (2 * size)) * tmp
    }
  }

  case class Normalization[A](mean: Matrix[A], standardDeviation: Matrix[A], result: Matrix[A])

  def featureNormalization(matrix: Matrix[Double]): Normalization[Double] = {
    val transposed = matrix.transpose
    val proc = transposed.elements.map(c => mean(c) -> standardDeviation(c))
    val result = Matrix(transposed.elements.zip(proc).map { case (data, (mean, std)) =>
      data.map(d => (d - mean) / std)
    }).transpose
    Normalization(Matrix(Seq(proc.map(_._1))), Matrix(Seq(proc.map(_._2))), result)
  }

  def standardDeviation(data: Seq[Double]): Double = {
    val size = fractionalSize(data)
    val mean = data.sum / size
    Math.sqrt(data.map(d => Matrix.scalarPower(d - mean,2)).sum / (size - 1))
  }

  def mean[A: Fractional](features: Seq[A]): A = {
    val size = features.foldLeft(implicitly[Fractional[A]].zero) {
      case (i,_) => i + implicitly[Fractional[A]].one
    }
    features.sum / size
  }

  def fractionalSize[A: Fractional](list: Seq[A]): A =
    list.foldLeft(implicitly[Fractional[A]].zero) {
      case (i,_) => i + implicitly[Fractional[A]].one
    }
}

object Util {

  def foo[F[_],A,B,C](fa: F[A], fb: F[B])(f: A => B => C): F[C] = ???
}
