package io.lbert.ml

import cats.implicits._
import scala.Numeric.Implicits._
import scala.Fractional.Implicits._

case class Row(underlying: Int) extends AnyVal
case class Column(underlying: Int) extends AnyVal

case class MatrixIndex(row: Row, column: Column) {
  override def toString: String = s"${row.underlying},${column.underlying}"
}

case class MatrixSize(rows: Row, columns: Column) {
  override def toString: String = s"${rows.underlying}x${columns.underlying}"
}
case class SquareMatrixSize(size: Int) {
  def toMatrixSize: MatrixSize = MatrixSize(Row(size), Column(size))
}
case class VectorIndex(row: Row)
case class VectorSize(rows: Row)

case class Matrix[A](elements: Seq[Seq[A]]) {
  override def toString: String =
    elements.map(_.map(_.toString).mkString(" ")).mkString("\n")
}


object Matrix {

  type MatrixResult[A] = ErrorOr[Matrix[A]]
  type ValueResult[A] = ErrorOr[A]

  case class DifferentSizeError(m1: Matrix[_], m2: Matrix[_])
    extends Exception(s"")

  case class ElementSizeError(size: MatrixSize, elements: Seq[_])
    extends Exception(s"Elements size of ${elements.size} didn't match matrix size of $size")

  case class NotSquareError(m: Matrix[_])
    extends Exception(s"Must be a square matrix, you supplied ${size(m)}")

  case class InverseUndefinedError(m: Matrix[_])
    extends Exception(s"")

  case class ColumnSizeNotRowSizeError(m1: Matrix[_], m2: Matrix[_])
    extends Exception(s"The column count of first matrix must equal the row count of the second, you supplied $m1 and $m2")

  case class DifferentRowsError(m1: Matrix[_], m2: Matrix[_])
    extends Exception(s"Matrices must have the same number of rows, you supplied ${size(m1)} and ${size(m2)}")

  case class DifferentColumnsError(m1: Matrix[_], m2: Matrix[_])
    extends Exception(s"Matrices must have the same number of columns, you supplied ${size(m1)} and ${size(m2)}")

  case object DivideByZeroError extends Exception("Can't divide matrix by 0")

  case class InvalidIndexError(index: MatrixIndex, m: Matrix[_])
    extends Exception(s"Index $index is not valid for matrix of size ${size(m)}")

  def add[A: Numeric](matrix1: Matrix[A], matrix2: Matrix[A]): MatrixResult[A] =
    combine(matrix1, matrix2)(_ + _)

  def add[A: Numeric](matrix: Matrix[A], a: A): MatrixResult[A] =
    Right(map(matrix)(_ + a))

  def subtract[A: Numeric](matrix1: Matrix[A], matrix2: Matrix[A]): MatrixResult[A] =
    combine(matrix1, matrix2)(_ - _)

  def subtract[A: Numeric](matrix: Matrix[A], a: A): MatrixResult[A] =
    Right(map(matrix)(_ - a))

  def subtract[A: Numeric](a: A, matrix: Matrix[A]): MatrixResult[A] =
    Right(map(matrix)(a - _))

  def multiply[A: Numeric](matrix: Matrix[A], a: A): MatrixResult[A] =
    Right(map(matrix)(_ * a))

  def multiply[A: Numeric](matrix1: Matrix[A], matrix2: Matrix[A]): MatrixResult[A] = {
    val matrix1Size = size(matrix1)
    val matrix2Size = size(matrix2)
    if(matrix1Size.columns.underlying != matrix2Size.rows.underlying) {
      Left(ColumnSizeNotRowSizeError(matrix1, matrix2))
    } else {
      val matrix2Columns = sliceColumns(matrix2)
      Right(Matrix(sliceRows(matrix1).map(matrix1Row =>
        matrix2Columns.map(matrix2Column =>
          matrix1Row.zip(matrix2Column).map(i => i._1 * i._2).sum
        )
      )))
    }
  }

  def power[A: Numeric](matrix: Matrix[A], _power: Int): MatrixResult[A] =
    Right(map(matrix)(scalarPower(_,_power)))

  def concat[A: Numeric](matrix1: Matrix[A], matrix2: Matrix[A]): MatrixResult[A] =
    if(size(matrix1).rows.underlying != size(matrix2).rows.underlying) {
      Left(DifferentRowsError(matrix1, matrix2))
    } else {
      Right(Matrix(matrix1.elements.zip(matrix2.elements).map(s => s._1 ++ s._2)))
    }

  def stack[A: Numeric](matrix1: Matrix[A], matrix2: Matrix[A]): MatrixResult[A] =
    if(size(matrix1).columns.underlying != size(matrix2).columns.underlying) {
      Left(DifferentColumnsError(matrix1, matrix2))
    } else {
      Right(Matrix(Seq(matrix1.elements, matrix2.elements).flatten))
    }

  def sum[A: Numeric](matrix: Matrix[A]): A =
    matrix.elements.flatten.sum

  def isSquare[A](matrix: Matrix[A]): Boolean = {
    val s = size(matrix)
    s.columns.underlying == s.rows.underlying
  }

  def squareCheck[A,B](matrix: Matrix[A]): ValueResult[Unit] =
    if(!isSquare(matrix)) {
      Left(NotSquareError(matrix))
    } else {
      Right()
    }

  def divide[A: Fractional](matrix: Matrix[A], a: A): MatrixResult[A] =
    if(a == implicitly[Fractional[A]].zero) Left(DivideByZeroError)
    else Right(map(matrix)(_ / a))

  def identity[A: Numeric](size: SquareMatrixSize): Matrix[A] = {
    val m = implicitly[Numeric[A]]
    mapWithIndex(fill(size.toMatrixSize,m.zero))((a,i) =>
      if(i.row.underlying == i.column.underlying) m.one else a
    )
  }

  def inverse[A: Fractional](matrix: Matrix[A]): MatrixResult[A] =
    if(!isSquare(matrix)) {
      Left(NotSquareError(matrix))
    } else {
      for {
        det <- determinant(matrix)
        out <- if(det == implicitly[Numeric[A]].zero) {
          Left(InverseUndefinedError(matrix))
        } else {
          adjoint(matrix).flatMap(divide(_,det))
        }
      } yield out
    }

  def adjoint[A: Numeric](matrix: Matrix[A]): MatrixResult[A] =
    for {
      _        <- squareCheck(matrix)
      cofactor <- cofactorMatrix(matrix)
    } yield transpose(cofactor)

  def determinant[A: Numeric](matrix: Matrix[A]): ValueResult[A] =
    for {
      _ <- squareCheck(matrix)
      s  = size(matrix)
      out <- if(s.columns.underlying == 1) {
        Right(matrix.elements.head.head)
      } else {
        resultSequence(
          getFirstRowIndices(matrix).map(index =>
            minor(matrix, index).map(cofactor(_,index) * get(matrix, index).get)
          )
        ).map(_.sum)
      }
    } yield out

  def minor[A: Numeric](matrix: Matrix[A], index: MatrixIndex): ValueResult[A] =
    for {
      _   <- squareCheck(matrix)
      _   <- indexExistsEither(matrix, index)
      sub <- removeRowColumn(matrix, index)
      out <- determinant(sub)
    } yield out

  def minorMatrix[A: Numeric](matrix: Matrix[A]): MatrixResult[A] =
    for {
      _        <- squareCheck(matrix)
      indexes   = getIndexes(matrix)
      elements <- resultSequence(indexes.map(minor(matrix,_)))
      out      <- build(size(matrix), elements)
    } yield out

  def cofactorMatrix[A: Numeric](matrix: Matrix[A]): MatrixResult[A] =
    for {
      _     <- squareCheck(matrix)
      minor <- minorMatrix(matrix)
    } yield mapWithIndex(minor)(cofactor[A])

  def cofactor[A: Numeric](a: A, index: MatrixIndex): A =
    scalarPower(implicitly[Numeric[A]].negate(implicitly[Numeric[A]].one),
      index.row.underlying + index.column.underlying
    ) * a

  private def resultSequence[A](s: Seq[ValueResult[A]]): ValueResult[Seq[A]] =
    s.toList.sequence.map(_.toSeq)

  private def getFirstRowIndices(matrix: Matrix[_]): Seq[MatrixIndex] =
    matrix.elements.headOption.map(_.indices.map(i =>
      MatrixIndex(Row(1), Column(i + 1)))).getOrElse(Seq.empty[MatrixIndex])

  def removeRowColumn[A](matrix: Matrix[A], index: MatrixIndex): MatrixResult[A] =
    for {
      _ <- squareCheck(matrix)
      _ <- indexExistsEither(matrix, index)
    } yield {
      Matrix(matrix.elements.zipWithIndex
        .filter{case (s,i) => (i + 1) != index.row.underlying}
        .map(_._1.zipWithIndex.filter{case (s,i) => (i + 1) != index.column.underlying}.map(_._1)))
    }

  def addFirstColumn[A](matrix: Matrix[A],a: A): Matrix[A] =
    transpose(Matrix(Seq.fill(rows(matrix).underlying)(a) +: transpose(matrix).elements))

  def getColumn[A](matrix: Matrix[A], column: Int): MatrixResult[A] =
    for {
      _ <- indexExistsEither(matrix, MatrixIndex(Row(1), Column(column)))
    } yield transpose(Matrix(Seq(transpose(matrix).elements(column))))

  def scalarPower[A: Numeric](a: A, p: Int): A =
    (0 until p).foldLeft(implicitly[Numeric[A]].one){ case (b, d) => b * a}

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
      Left(DifferentSizeError(matrix1, matrix2))
    } else {
      Right(Matrix(getIndexes(matrix1Size).flatMap {index =>
        for {
          i1 <- get(matrix1, index)
          i2 <- get(matrix2, index)
        } yield f(i1,i2)
      }.grouped(matrix1Size.columns.underlying).toSeq))
    }
  }

  def build[A](size: MatrixSize, elements: Seq[A]): MatrixResult[A] = {
    if(elements.size != size.columns.underlying * size.rows.underlying) {
      Left(ElementSizeError(size, elements))
    } else {
      Right(Matrix[A](elements.grouped(size.columns.underlying).toSeq))
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

  def indexExists(matrix: Matrix[_], index: MatrixIndex): Boolean =
    index.row.underlying <= rows(matrix).underlying && index.column.underlying <= columns(matrix).underlying

  def indexExistsEither(matrix: Matrix[_], index: MatrixIndex): ValueResult[Unit] =
    if(indexExists(matrix, index)) Right(()) else Left(InvalidIndexError(index, matrix))

  def get[A](matrix: Matrix[A], index: MatrixIndex): Option[A] =
    if(!indexExists(matrix,index)) None
    else Some(matrix.elements(index.row.underlying - 1)(index.column.underlying - 1))

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

  def takeRows[A](matrix: Matrix[A], n: Int): Matrix[A] =
    Matrix(matrix.elements.take(n))
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
