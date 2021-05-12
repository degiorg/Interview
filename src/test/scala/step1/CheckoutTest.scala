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
  """Checkout with 2 Apple""" should """be 0.60p""" in {
    Checkout(List("Apple", "Apple")) shouldEqual 0.60
  }
  """Checkout with 2 Apple and 1 Orange""" should """be 0.85p""" in {
    Checkout(List("Apple", "Apple", "Orange")) shouldEqual 0.85
  }
  """Checkout with Orange Apple Orange""" should """be 1.10p""" in {
    Checkout(List("Orange", "Apple", "Orange")) shouldEqual 1.10
  }
  """Checkout with 3 Orange """ should """be 0.5p""" in {
    Checkout(List("Orange", "Orange", "Orange")) shouldEqual 0.50
  }
  """Checkout with 6 Orange and 1 Apple """ should """be 1""" in {
    Checkout(List("Orange", "Orange", "Orange", "Apple", "Orange", "Orange", "Orange")) shouldEqual 1.60
  }
  """Checkout with 2 Apple and 1 Banana""" should """be 0.80p""" in {
    Checkout(List("Apple", "Apple", "Banana")) shouldEqual 0.80
  }
  """Checkout with 2 Banana and 1 Apple""" should """be 0.80p""" in {
    Checkout(List("Banana", "Apple", "Banana")) shouldEqual 0.80
  }

  """Checkout with 2 Banana and 2 Apple""" should """be 0.80p""" in {
    Checkout(List("Banana", "Apple", "Banana", "Apple")) shouldEqual 0.80
  }
  """Checkout with 7 Banana and 2 Apple""" should """be 1.40p""" in {
    Checkout(List("Banana", "Banana", "Banana", "Banana", "Banana", "Banana", "Apple", "Banana", "Apple")) shouldEqual 1.40
  }

}
