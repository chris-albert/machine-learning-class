package io.lbert.ml

import cats.implicits._
import io.lbert.ml.MathableSyntax._

case class Row(underlying: Int) extends AnyVal
case class Column(underlying: Int) extends AnyVal

case class MatrixIndex(row: Row, column: Column)
case class MatrixSize(rows: Row, columns: Column) {
  override def toString: String = s"${rows.underlying}x${columns.underlying}"
}
case class SquareMatrixSize(size: Int) {
  def toMatrixSize: MatrixSize = MatrixSize(Row(size), Column(size))
}
case class VectorIndex(row: Row)
case class VectorSize(rows: Row)

case class Matrix[A](elements: Seq[Seq[A]])

object Matrix {

  type MatrixResult[A] = Either[String, Matrix[A]]

  def add[A: Mathable](matrix1: Matrix[A], matrix2: Matrix[A]): MatrixResult[A] =
    combine(matrix1, matrix2)(_ + _)

  def subtract[A: Mathable](matrix1: Matrix[A], matrix2: Matrix[A]): MatrixResult[A] =
    combine(matrix1, matrix2)(_ - _)

  def multiply[A: Mathable](matrix: Matrix[A], a: A): MatrixResult[A] =
    Right(map(matrix)(_ * a))

  def multiply[A: Mathable](matrix1: Matrix[A], matrix2: Matrix[A]): MatrixResult[A] = {
    val matrix1Size = size(matrix1)
    val matrix2Size = size(matrix2)
    if(matrix1Size.columns.underlying != matrix2Size.rows.underlying) {
      Left(s"The column count of first matrix must equal the row count of the second, you supplied $matrix1Size and $matrix2Size")
    } else {
      val matrix2Columns = sliceColumns(matrix2)
      Right(Matrix(sliceRows(matrix1).map(matrix1Row =>
        matrix2Columns.map(matrix2Column =>
          matrix1Row.zip(matrix2Column).map(i => i._1 * i._2).reduceLeft(_ + _)
        )
      )))
    }
  }

  def isSquare[A](matrix: Matrix[A]): Boolean = {
    val s = size(matrix)
    s.columns.underlying == s.rows.underlying
  }

  def divide[A: Mathable](matrix: Matrix[A], a: A): MatrixResult[A] =
    if(a == 0) Left("Divide by 0 undefined")
    else Right(map(matrix)(_ / a))

  def identity[A: Mathable](size: SquareMatrixSize): Matrix[A] = {
    val m = implicitly[Mathable[A]]
    mapWithIndex(fill(size.toMatrixSize,m.zero))((a,i) =>
      if(i.row.underlying == i.column.underlying) m.one else a
    )
  }

  def inverse[A: Mathable](matrix: Matrix[A]): MatrixResult[A] = {
    if(!isSquare(matrix)) {
      Left(s"Can only get inverse of square matrix, you supplied ${size(matrix)}")
    } else {
      val det = determinant(matrix)
      if(det == implicitly[Mathable[A]].zero) {
        Left(s"Determinate of matrix is 0")
      } else {
        divide(adjoint(matrix), det)
      }
    }
  }

  def adjoint[A](matrix: Matrix[A]): Matrix[A] = ???

  def determinant[A](matrix: Matrix[A]): A = ???

  def transpose[A](matrix: Matrix[A]): Matrix[A] =
    Matrix(sliceColumns(matrix))

  def sliceRows[A](matrix: Matrix[A]): Seq[Seq[A]] = matrix.elements

  def sliceColumns[A](matrix: Matrix[A]): Seq[Seq[A]] = {
    val s = size(matrix)
    (1 to s.columns.underlying).map(column =>
      (1 to s.rows.underlying).flatMap(row =>
        get(matrix, MatrixIndex(Row(row), Column(column)))
      )
    )
  }

  def combine[A,B](matrix1: Matrix[A], matrix2: Matrix[A])(f: (A,A) => B): MatrixResult[B] = {
    val matrix1Size = size(matrix1)
    val matrix2Size = size(matrix2)
    if(matrix1Size != matrix2Size) {
      Left(s"Matrices are not the same size. $matrix1Size and $matrix2Size are not equal")
    } else {
      Right(Matrix(getIndexes(matrix1Size).flatMap {index =>
        for {
          i1 <- get(matrix1, index)
          i2 <- get(matrix2, index)
        } yield f(i1,i2)
      }.grouped(matrix1Size.columns.underlying).toSeq))
    }
  }

  def map[A,B](matrix: Matrix[A])(f: A => B): Matrix[B] =
    Matrix(getIndexes(matrix).flatMap{index =>
      get(matrix, index).map(f)
    }.grouped(size(matrix).columns.underlying).toSeq)

  def mapWithIndex[A,B](matrix: Matrix[A])(f: (A, MatrixIndex) => B): Matrix[B] =
    Matrix(getIndexes(matrix).flatMap{index =>
      get(matrix, index).map(f(_, index))
    }.grouped(size(matrix).columns.underlying).toSeq)

  def get[A](matrix: Matrix[A], index: MatrixIndex): Option[A] =
    if(index.row.underlying > rows(matrix).underlying || index.column.underlying > columns(matrix).underlying) {
      None
    } else {
      Some(matrix.elements(index.row.underlying - 1)(index.column.underlying - 1))
    }

  def getIndexes(size: MatrixSize): Seq[MatrixIndex] =
    for {
      row    <- 1 to size.rows.underlying
      column <- 1 to size.columns.underlying
    } yield MatrixIndex(Row(row),Column(column))

  def getIndexes(matrix: Matrix[_]): Seq[MatrixIndex] = getIndexes(size(matrix))

  def dimension(matrix: Matrix[_]): Int = {
    val s = size(matrix)
    s.rows.underlying * s.columns.underlying
  }

  def size(matrix: Matrix[_]): MatrixSize =
    MatrixSize(rows(matrix), columns(matrix))

  def rows(matrix: Matrix[_]): Row =
    Row(matrix.elements.size)

  def columns(matrix: Matrix[_]): Column =
    Column(matrix.elements.headOption.map(_.size).getOrElse(0))

  def fillFunc[A](size: MatrixSize, fillFunc: MatrixIndex => A): Matrix[A] =
    Matrix((1 to size.rows.underlying).map(row =>
      (1 to size.columns.underlying).map(column =>
        fillFunc(MatrixIndex(Row(row),Column(column)))
      )
    ))

  def fill[A](size: MatrixSize, value: A): Matrix[A] =
    fillFunc(size,_ => value)

  def empty[A]: Matrix[A] = Matrix[A](Seq.empty[Seq[A]])
}

object Vector {

  def empty[A]: Matrix[A] = Matrix.empty[A]

  def apply[A](elements: Seq[A]): Matrix[A] =
    Matrix(elements.map(Seq(_)))

  def fill[A](size: VectorSize, value: A): Matrix[A] =
    Matrix.fill[A](MatrixSize(size.rows,Column(1)),value)

  def fillFunc[A](size: VectorSize, fillFunc: VectorIndex => A): Matrix[A] =
    Matrix.fillFunc[A](MatrixSize(size.rows,Column(1)),mi => fillFunc(VectorIndex(mi.row)))
}
