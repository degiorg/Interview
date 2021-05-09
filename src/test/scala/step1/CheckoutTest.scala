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
  Checkout(List("Apple")) shouldEqual 0.00
}
"""Checkout with an Orange""" should """be 25p""" in {
    Checkout(List("Orange")) shouldEqual 0.25
  }
  """Checkout with 2 Apple""" should """be 0.60p""" in {
    Checkout(List("Apple", "Apple")) shouldEqual 0.60
  }
  """Checkout with 2 Apple and 1 Orange""" should """be 0.85p""" in {
    Checkout(List("Apple", "Apple", "Orange")) shouldEqual 0.85
  }
  """Checkout with Orange Apple Orange""" should """be 0.50p""" in {
    Checkout(List("Orange", "Apple", "Orange")) shouldEqual 0.50
  }
  """Checkout with 3 Orange """ should """be 0.5p""" in {
    Checkout(List("Orange", "Orange", "Orange")) shouldEqual 0.50
  }
  """Checkout with 6 Orange and 1 Apple """ should """be 1""" in {
    Checkout(List("Orange", "Orange", "Orange", "Apple", "Orange", "Orange", "Orange")) shouldEqual 1.00
  }
}
