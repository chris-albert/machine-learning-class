package io.lbert.ml.implicits

import io.lbert.ml.Matrix
import io.lbert.ml.Matrix.MatrixResult

object MatrixSyntax {

  implicit class MatrixNumericOps[A : Numeric](matrix: Matrix[A]) {

    def *(other: Matrix[A]): MatrixResult[A] =
      Matrix.multiply(matrix, other)

    def *(scalar: A): MatrixResult[A] =
      Matrix.multiply(matrix, scalar)

    def +(other: Matrix[A]): MatrixResult[A] =
      Matrix.add(matrix, other)

    def +(scalar: A): MatrixResult[A] =
      Matrix.add(matrix, scalar)

    def -(other: Matrix[A]): MatrixResult[A] =
      Matrix.subtract(matrix, other)

    def -(scalar: A): MatrixResult[A] =
      Matrix.subtract(matrix, scalar)

    def transpose: Matrix[A] =
      Matrix.transpose(matrix)

    def map[B](f: A => B): Matrix[B] =
      Matrix.map(matrix)(f)

    def sum: A =
      Matrix.sum(matrix)
  }

  implicit class MatrixFractionalOps[A : Fractional](matrix: Matrix[A]) {

    def /(scalar: A): MatrixResult[A] =
      Matrix.divide(matrix, scalar)
  }

  implicit class NumericOps[A: Numeric](a: A) {

    def -(matrix: Matrix[A]): MatrixResult[A] =
      Matrix.subtract(a, matrix)
  }
}
