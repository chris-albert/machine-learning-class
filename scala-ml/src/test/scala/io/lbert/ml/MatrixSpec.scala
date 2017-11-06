package io.lbert.ml

import org.scalatest.{Matchers, WordSpec}

class MatrixSpec extends WordSpec with Matchers {

  "empty" should {
    "create empty matrix" in {
      Matrix.empty[Int] shouldBe Matrix(Seq.empty[Seq[Int]])
    }
  }

  "dimension" should {
    "get dimension of 0 element matrix" in {
      Matrix.dimension(Matrix(Seq.empty[Seq[Int]])) shouldBe 0
    }
    "get dimension of 5x5 matrix" in {
      val m = Matrix.fill(MatrixSize(Row(5),Column(5)),1)
      Matrix.dimension(m) shouldBe 25
    }
  }

  "size" should {
    "get size of 0 element matrix" in {
      Matrix.size(Matrix.empty[Int]) shouldBe MatrixSize(Row(0), Column(0))
    }
    "get size of 5x4 element matrix" in {
      Matrix.size(Matrix.fill(MatrixSize(Row(5), Column(4)),1)) shouldBe MatrixSize(Row(5), Column(4))
    }
  }

  "rows" should {
    "return no rows in empty matrix" in {
      Matrix.rows(Matrix.empty[Int]) shouldBe Row(0)
    }
    "return 5 rows in 5x4 matrix" in {
      Matrix.rows(Matrix.fill(MatrixSize(Row(5), Column(4)),1)) shouldBe Row(5)
    }
  }

  "columns" should {
    "columns no columns in empty matrix" in {
      Matrix.columns(Matrix.empty[Int]) shouldBe Column(0)
    }
    "return 4 columns in 5x4 matrix" in {
      Matrix.columns(Matrix.fill(MatrixSize(Row(5), Column(4)),1)) shouldBe Column(4)
    }
  }

  "fill" should {
    "fill 0 element matrix" in {
      Matrix.fill(MatrixSize(Row(0),Column(0)),1) shouldBe Matrix(
        Seq.empty[Seq[Int]]
      )
    }
    "fill 3x3 element matrix" in {
      Matrix.fill(MatrixSize(Row(3),Column(3)),1) shouldBe Matrix(
        Seq(
          Seq(1,1,1),
          Seq(1,1,1),
          Seq(1,1,1)
        )
      )
    }
  }

  "fillFunc" should {
    "fill 3x3 element matrix with index func" in {
      Matrix.fillFunc[Int](MatrixSize(Row(3),Column(3)),mi => mi.row.underlying * mi.column.underlying) shouldBe Matrix(
        Seq(
          Seq(1,2,3),
          Seq(2,4,6),
          Seq(3,6,9)
        )
      )
    }
  }

  "add" should {
    "fail for matrices that aren't the same size" in {
      Matrix.add(
        Matrix.fill(MatrixSize(Row(2),Column(2)),1),
        Matrix.fill(MatrixSize(Row(3),Column(2)),2)
      ) shouldBe Left("Matrices are not the same size. 2x2 and 3x2 are not equal")
    }
    "add matrices" in {
      Matrix.add(
        Matrix.fill(MatrixSize(Row(3),Column(2)),1),
        Matrix.fill(MatrixSize(Row(3),Column(2)),2)
      ) shouldBe Right(Matrix.fill(MatrixSize(Row(3),Column(2)),3))
      1
    }
  }

  "get" should {
    val twoByTwoMatrix = Matrix.fillFunc(MatrixSize(Row(2),Column(2)),mi => mi.row.underlying * mi.column.underlying)
    "fail if index doesn't exist" in {
      Matrix.get(twoByTwoMatrix, MatrixIndex(Row(3), Column(1))) shouldBe None
      Matrix.get(twoByTwoMatrix, MatrixIndex(Row(1), Column(3))) shouldBe None
    }
    "fetch dimension 1x1 element" in {
      Matrix.get(twoByTwoMatrix, MatrixIndex(Row(1), Column(1))) shouldBe Some(1)
    }
    "fetch dimension 2x2" in {
      Matrix.get(twoByTwoMatrix, MatrixIndex(Row(2), Column(2))) shouldBe Some(4)
    }
  }

  "getIndexes" should {
    "get 4 indexes for a 2x2 matrix" in {
      val twoByTwoMatrix = Matrix.fill(MatrixSize(Row(2),Column(2)),1)
      Matrix.getIndexes(twoByTwoMatrix) shouldBe Seq(
        MatrixIndex(Row(1), Column(1)),
        MatrixIndex(Row(1), Column(2)),
        MatrixIndex(Row(2), Column(1)),
        MatrixIndex(Row(2), Column(2))
      )
    }
  }
}
