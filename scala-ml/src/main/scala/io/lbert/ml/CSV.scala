package io.lbert.ml

import io.lbert.ml.fp.IO
import scala.io.{BufferedSource, Source}

trait CSV {

  type IOResult[A] = IO[ErrorOr[A]]

  def read(fileName: String): IO[List[List[String]]] =
    readRaw(fileName).map(parse)

  def readDouble(fileName: String): IO[List[List[Double]]] =
    readRaw(fileName).map(parse).map(_.map(_.map(_.toDouble)))

  def readMatrix(fileName: String): IO[Matrix[Double]] =
    readDouble(fileName).map(Matrix(_))

  def readRaw(fileName: String): IO[BufferedSource] =
    IO(Source.fromResource(fileName))

  def parse(source: BufferedSource): List[List[String]] =
    source.getLines().map(_.split(",").toList).toList
}

object CSV extends CSV
