package step1
import step1.Checkout._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers._

class CheckoutTest extends AnyFlatSpec  with TypeCheckedTripleEquals {
"""Checkout with an empty list""" should """be 0""" in {
  Checkout(List("")) shouldEqual 0
}
"""Checkout with an Apple""" should """be 60p""" in {
  Checkout(List("Apple")) shouldEqual 0.60
}
"""Checkout with an Orange""" should """be 25p""" in {
    Checkout(List("Orange")) shouldEqual 0.25
  }
  """Checkout with 2 Apple""" should """be 1.20p""" in {
    Checkout(List("Apple", "Apple")) shouldEqual 1.20
  }
  """Checkout with 2 Apple and 1 Orange""" should """be 1.20p""" in {
    Checkout(List("Apple", "Apple", "Orange")) shouldEqual 1.45
  }
  """Checkout with Orange Apple Orange""" should """be 1.10p""" in {
    Checkout(List("Orange", "Apple", "Orange")) shouldEqual 1.10
  }
}
