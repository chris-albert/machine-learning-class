package io.lbert.ml.fp

import org.scalatest.{Matchers, WordSpec}
import io.lbert.ml.fp.MonadInstances._

class FunctorSpec extends WordSpec with Matchers {

  val listOption = List(Some(1), None, Some(2))

  Functor[List].compose[Option].map(listOption)(_ + 1)


}
