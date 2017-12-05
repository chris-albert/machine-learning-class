package io.lbert.ml.implicits

import io.lbert.ml.{Column, Matrix, MatrixSize, Row}
import org.scalatest.{Matchers, WordSpec}
import io.lbert.ml.implicits.MatrixSyntax._

class MatrixSyntaxSpec extends WordSpec with Matchers {

  val matrix1: Matrix[Int] = Matrix.fill(MatrixSize(Row(3),Column(3)), 3)
  val matrix2: Matrix[Int] = Matrix.fill(MatrixSize(Row(3),Column(3)), 4)
  val matrixDouble: Matrix[Double] = Matrix.fill(MatrixSize(Row(3),Column(3)), 4)

  "MatrixOps" should {
    "multiply by another matrix" in {
      matrix1 * matrix2 shouldBe Matrix.multiply(matrix1, matrix2)
    }
    "multiply by a scalar" in {
      matrix1 * 2 shouldBe Matrix.multiply(matrix1, 2)
    }
    "add another matrix" in {
      matrix1 + matrix2 shouldBe Matrix.add(matrix1, matrix2)
    }
    "add scalar value" in {
      matrix1 + 10 shouldBe Matrix.add(matrix1, 10)
    }
    "subtract another matrix" in {
      matrix1 - matrix2 shouldBe Matrix.subtract(matrix1, matrix2)
    }
    "subtract scalar value" in {
      matrix1 - 2 shouldBe Matrix.subtract(matrix1, 2)
    }
    "subtract from scalar value" in {
      2 - matrix1 shouldBe Matrix.subtract(2, matrix1)
    }
    "divide by a scalar" in {
      matrixDouble / 3.0 shouldBe Matrix.divide(matrixDouble, 3.0)
    }
    "transpose a matrix" in {
      matrix1.transpose shouldBe Matrix.transpose(matrix1)
    }
  }
}