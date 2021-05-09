package step1

object Checkout {
  def apply (items: List[String]): Double = {
    val chart = Chart(items.groupMapReduce(identity)(_ => 1)(_ + _))
    chart.chartList.foldLeft (Total.empty) {
      case (tot , currItem ) =>
        currItem match {
          case ("Apple", _) => Apple(currItem._2).subTotal + tot
          case ("Orange", _) => Orange(currItem._2).subTotal + tot
          case _ => 0
        }
    }
    }
}

case class Chart (chartList: Map[String, Int])
case class Total(price: Double)
object Total {
  val empty = 0.0
}

trait Item {
  def cost: Double
  def num: Int
  def discount : Double
  def subTotal: Double = (cost * num) - discount

}

case class Apple(num: Int) extends Item {
  override val cost = 0.60
  override val discount: Double = cost
}

case class Orange(num: Int) extends Item{
  override val cost = 0.25
  override def discount: Double = {
    num / 3 match {
      case res if res >= 1 => res * cost
      case _ => 0
    }
  }
}

