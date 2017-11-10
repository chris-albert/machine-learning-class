package io.lbert.ml

import org.scalatest.{Matchers, WordSpec}
import io.lbert.ml.MathableInstances._

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

  "map" should {
    "apply function to each element" in {
      Matrix.map(Matrix.fill(MatrixSize(Row(2),Column(2)),1))(_ + "hi") shouldBe
        Matrix.fill(MatrixSize(Row(2),Column(2)),"1hi")
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
    }
  }

  "subtract" should {
    "subtract matrices" in {
      Matrix.subtract(
        Matrix.fill(MatrixSize(Row(3),Column(2)),1),
        Matrix.fill(MatrixSize(Row(3),Column(2)),2)
      ) shouldBe Right(Matrix.fill(MatrixSize(Row(3),Column(2)),-1))
    }
  }

  "scalar multiply" should {
    "multiply by scalar value" in {
      Matrix.multiply(
        Matrix.fill(MatrixSize(Row(3),Column(2)),2), 3
      ) shouldBe Right(Matrix.fill(MatrixSize(Row(3),Column(2)),6))
    }
  }

  "scalar divide" should {
    "fail if 0" in {
      Matrix.divide(
        Matrix.fill(MatrixSize(Row(3),Column(2)),2), 0
      ) shouldBe Left("Divide by 0 undefined")
    }
    "divide by scalar value" in {
      Matrix.divide(
        Matrix.fill(MatrixSize(Row(3),Column(2)),10), 2
      ) shouldBe Right(Matrix.fill(MatrixSize(Row(3),Column(2)),5))
    }
  }

  "slice rows" should {
    "give back rows" in {
      Matrix.sliceRows(
        Matrix(
          Seq(
            Seq(1,2),
            Seq(3,4),
            Seq(5,6)
          )
        )
      ) shouldBe Seq(
        Seq(1,2),
        Seq(3,4),
        Seq(5,6)
      )
    }
  }

  "slice columns" should {
    "give back columns" in {
      Matrix.sliceColumns(
        Matrix(
          Seq(
            Seq(1,2),
            Seq(3,4),
            Seq(5,6)
          )
        )
      ) shouldBe Seq(
        Seq(1,3,5),
        Seq(2,4,6)
      )
    }
  }

  "multiply" should {
    "fail if matrix isn't the right size" in {
      Matrix.multiply(
        Matrix.fill(MatrixSize(Row(3),Column(2)),1),
        Matrix.fill(MatrixSize(Row(5),Column(5)),2)
      ) shouldBe Left("The column count of first matrix must equal the row count of the second, you supplied 3x2 and 5x5")
    }
    "succeed" in {
      Matrix.multiply(
        Matrix[Double](
          Seq(
            Seq(1,2),
            Seq(3,4),
            Seq(5,6)
          )
        ),
        Matrix[Double](
          Seq(
            Seq(1,2),
            Seq(3,4)
          )
        )
      ) shouldBe Right(
        Matrix[Double](
          Seq(
            Seq(7,10),
            Seq(15,22),
            Seq(23,34)
          )
        )
      )
    }
  }

  "identity" should {
    "create a 5x5 identity matrix" in {
      Matrix.identity[Int](SquareMatrixSize(5)) shouldBe Matrix(
        Seq(
          Seq(1,0,0,0,0),
          Seq(0,1,0,0,0),
          Seq(0,0,1,0,0),
          Seq(0,0,0,1,0),
          Seq(0,0,0,0,1)
        )
      )
    }
  }

  "transpose" should {
    "transpose matrix" in {
      Matrix.transpose(
        Matrix(
          Seq(
            Seq(1,2),
            Seq(3,4),
            Seq(5,6)
          )
        )
      ) shouldBe Matrix[Double](
        Seq(
          Seq(1,3,5),
          Seq(2,4,6)
        )
      )
    }
  }

  "inverse" should {
    "fail if not square matrix" in {
      Matrix.inverse(
        Matrix.fill(MatrixSize(Row(3),Column(2)),1)
      ) shouldBe Left("Can only get inverse of square matrix, you supplied 3x2")
    }
//    "get inverse matrix" in {
//      Matrix.inverse(
//        Matrix(
//          Seq(
//            Seq(1,3,3),
//            Seq(1,4,3),
//            Seq(1,3,4)
//          )
//        )
//      ) shouldBe Matrix[Double](
//        Seq(
//          Seq(7,-3,-3),
//          Seq(-1,1,0),
//          Seq(-1,0,1)
//        )
//      )
//    }
  }

  "determinant" should {
    "fail if not square matrix" in {
      Matrix.determinant(
        Matrix.fill(MatrixSize(Row(3),Column(2)),1)
      ) shouldBe Left("Must be a square matrix, you supplied 3x2")
    }
    "get determinate of 2x2 matrix" in {
      Matrix.determinant(
        Matrix(
          Seq(
            Seq(1,4),
            Seq(-1,9)
          )
        )
      ) shouldBe Right(13)
    }
    "get determinate of 3x3 matrix" in {
      Matrix.determinant(
        Matrix(
          Seq(
            Seq(3,2,1),
            Seq(2,1,-3),
            Seq(4,0,1)
          )
        )
      ) shouldBe Right(-29)
    }
    "get determinate of 4x4 matrix" in {
      Matrix.determinant(
        Matrix(
          Seq(
            Seq(3,2,-1,4),
            Seq(2,1,5,7),
            Seq(0,5,2,-6),
            Seq(-1,2,1,0)
          )
        )
      ) shouldBe Right(-418)
    }
  }

  "removeIndexRowColumn" should {
    val threeByThree = Matrix(
      Seq(
        Seq(1,2,3),
        Seq(4,5,6),
        Seq(7,8,9)
      )
    )
    "get row 1 col 2 minor of 3x3" in {
      Matrix.removeIndexRowColumn(
        threeByThree,
        MatrixIndex(Row(1), Column(2))
      ) shouldBe Right(Matrix(
        Seq(
          Seq(4,6),
          Seq(7,9)
        )
      ))
    }
    "get row 1 col 1 minor of 3x3" in {
      Matrix.removeIndexRowColumn(
        threeByThree,
        MatrixIndex(Row(1), Column(1))
      ) shouldBe Right(Matrix(
        Seq(
          Seq(5,6),
          Seq(8,9)
        )
      ))
    }
    "get row 3 col 3 minor of 3x3" in {
      Matrix.removeIndexRowColumn(
        threeByThree,
        MatrixIndex(Row(3), Column(3))
      ) shouldBe Right(Matrix(
        Seq(
          Seq(1,2),
          Seq(4,5)
        )
      ))
    }
    "get row 2 col 2 minor of 3x3" in {
      Matrix.removeIndexRowColumn(
        threeByThree,
        MatrixIndex(Row(2), Column(2))
      ) shouldBe Right(Matrix(
        Seq(
          Seq(1,3),
          Seq(7,9)
        )
      ))
    }
  }

  "pow" should {
    "get power of 5" in {
      Matrix.pow(5,5) shouldBe 3125
    }
  }

//  "cofactor" should {
//    "get cofactor o"
//  }
}
