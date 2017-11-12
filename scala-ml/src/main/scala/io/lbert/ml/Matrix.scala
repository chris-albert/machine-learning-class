package io.lbert.ml

import cats.implicits._
import io.lbert.ml.MathableSyntax._

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

  type Error = String
  type MatrixResult[A] = Either[Error, Matrix[A]]
  type ValueResult[A] = Either[Error, A]

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

  def inverse[A: Mathable](matrix: Matrix[A]): MatrixResult[A] =
    if(!isSquare(matrix)) {
      Left(s"Can only get inverse of square matrix, you supplied ${size(matrix)}")
    } else {
      for {
        det <- determinant(matrix).right
        out <- if(det == implicitly[Mathable[A]].zero) {
          Left(s"Can't get inverse of matrix since determinate of matrix is 0")
        } else {
          adjoint(matrix).right.flatMap(divide(_,det))
        }
      } yield out
    }

  def squareCheck[A,B](matrix: Matrix[A]): ValueResult[Unit] =
    if(!isSquare(matrix)) {
      Left(s"Must be a square matrix, you supplied ${size(matrix)}")
    } else {
      Right()
    }

  def adjoint[A: Mathable](matrix: Matrix[A]): MatrixResult[A] =
    for {
      _        <- squareCheck(matrix).right
      cofactor <- cofactorMatrix(matrix).right
    } yield transpose(cofactor)

  def determinant[A: Mathable](matrix: Matrix[A]): ValueResult[A] = {
    for {
      _ <- squareCheck(matrix).right
      s  = size(matrix)
      out <-  if(s.columns.underlying == 2) {
        determinat2x2(matrix).right
      } else {
        val firstRowIndexes = getFirstRowIndices(matrix)
        val result: Seq[ValueResult[A]] = firstRowIndexes.map { index =>
          for {
            nm <- removeIndexRowColumn(matrix, index).right
            d  <- determinant(nm).right
          } yield get(matrix, index).get * cofactor(d, index)
        }
        resultSequence(result).right.map[A](_.foldLeft(implicitly[Mathable[A]].zero)(_ + _)).right
      }
    } yield out
  }

  private def resultSequence[A](s: Seq[ValueResult[A]]): ValueResult[Seq[A]] = {

    def loop(result: List[ValueResult[A]], accu: ValueResult[List[A]]): ValueResult[Seq[A]] = {
      result match {
        case Nil => accu
        case Right(x) :: xs =>
          loop(xs, accu.right.map(r => x :: r))
        case Left(x) :: xs =>
          Left(x)
      }
    }
    loop(s.toList, Right(List.empty[A])).right.map(_.reverse)
  }

  private def determinat2x2[A: Mathable](matrix: Matrix[A]): ValueResult[A] = {
    val s = size(matrix)
    if(s.columns.underlying == 2) {
      (for {
        a <- get(matrix,MatrixIndex(Row(1), Column(1)))
        b <- get(matrix,MatrixIndex(Row(1), Column(2)))
        c <- get(matrix,MatrixIndex(Row(2), Column(1)))
        d <- get(matrix,MatrixIndex(Row(2), Column(2)))
      } yield (a * d) - (b * c)).fold[ValueResult[A]](Left("Crazy error"))(Right(_))
    } else {
      Left("Must be 2x2 matrix")
    }
  }

  private def getFirstRowIndices(matrix: Matrix[_]): Seq[MatrixIndex] =
    matrix.elements.headOption.map(_.indices.map(i =>
      MatrixIndex(Row(1), Column(i + 1)))).getOrElse(Seq.empty[MatrixIndex])

  def removeIndexRowColumn[A](matrix: Matrix[A], index: MatrixIndex): MatrixResult[A] =
    for {
      _ <- squareCheck(matrix).right
      _ <- indexExistsEither(matrix, index).right
    } yield {
      Matrix(matrix.elements.zipWithIndex
        .filter{case (s,i) => (i + 1) != index.row.underlying}
        .map(_._1.zipWithIndex.filter{case (s,i) => (i + 1) != index.column.underlying}.map(_._1)))
    }

  def minor[A: Mathable](matrix: Matrix[A], index: MatrixIndex): ValueResult[A] =
    for {
      _ <- squareCheck(matrix).right
      _ <- indexExistsEither(matrix, index).right
      _size = size(matrix)
      out <- if(_size.columns.underlying == 2) {
        removeIndexRowColumn(matrix, index).right.map(_.elements.head.head)
      } else {
        removeIndexRowColumn(matrix, index).right.flatMap { matrix =>
          determinant(matrix)
        }
      }
    } yield out

  def minorMatrix[A: Mathable](matrix: Matrix[A]): MatrixResult[A] =
    for {
      _        <- squareCheck(matrix).right
      indexes   = getIndexes(matrix)
      elements <- resultSequence(indexes.map(minor(matrix,_))).right
      out      <- build(size(matrix), elements).right
    } yield out

  def cofactorMatrix[A: Mathable](matrix: Matrix[A]): MatrixResult[A] =
    for {
      _     <- squareCheck(matrix).right
      minor <- minorMatrix(matrix).right
    } yield mapWithIndex(minor)(cofactor[A])

  def cofactor[A: Mathable](a: A, index: MatrixIndex): A =
    pow(implicitly[Mathable[A]].negOne,index.row.underlying + index.column.underlying) * a

  def pow[A: Mathable](a: A, p: Int): A =
    (0 until p).foldLeft(implicitly[Mathable[A]].one){ case (b, d) => b * a}

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

  def build[A](size: MatrixSize, elements: Seq[A]): MatrixResult[A] = {
    if(elements.size != size.columns.underlying * size.rows.underlying) {
      Left(s"Elements size of ${elements.size} didn't match matrix size of $size")
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
    if(indexExists(matrix, index)) Right(()) else Left(s"Index $index is not valid for matrix of size ${size(matrix)}")

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
