package step1

object Checkout {
  def apply (items: List[String]): BigDecimal = {
    val chart = Basket(items.groupMapReduce(identity)(_ => 1)(_ + _))
    val chartToProcess = chart.chartList.getOrElse(Apple.name, 1) > 1 && chart.chartList.getOrElse(Banana.name, 1) > 1 match {
      case true =>
        Apple(chart.chartList(Apple.name)) > Banana(chart.chartList(Banana.name) ) match {
          case true => Basket(chart.chartList + (Banana.name -> chart.chartList(Banana.name).-(1)))
          case _ => Basket(chart.chartList + (Apple.name -> chart.chartList(Apple.name).-(1)))
        }
      case _ => Basket(chart.chartList)
    }
    chartToProcess.chartList.foldLeft (Total.empty) {
      case (tot , currItem ) =>
        currItem match {
          case (Apple.name, _) =>  Apple(currItem._2).subTotal + tot
          case (Orange.name, _) => Orange(currItem._2).subTotal + tot
          case (Banana.name, _) => Banana(currItem._2).subTotal + tot
          case _ => tot
        }
    }
  }
}

case class Basket (chartList: Map[String, Int]) extends AnyVal
case class Total(price: BigDecimal)
object Total {
  val empty: BigDecimal = 0.0
}

trait Item extends Ordered[Item] {
  def cost: BigDecimal
  def num: Int
  def discount : BigDecimal = 0
  def subTotal: BigDecimal = (cost * num) - discount
  def compare(that: Item): Int = {
    this.subTotal - that.subTotal match {
      case res if res >= 0 => 1
      case _ => -1
    }
  }
}

case class Apple(num: Int) extends Item {
  override val cost = 0.60
  override val discount: BigDecimal = Discount(2, num, cost).discount
}
object Apple {
  val name = "Apple"
}

case class Banana(num: Int) extends Item {
  override val cost = 0.20
  override val discount: BigDecimal = Discount(2, num, cost).discount
}
object Banana {
  val name = "Banana"
}

case class Orange(num: Int) extends Item  {
  override val cost = 0.25
  override val discount: BigDecimal = Discount(3, num, cost).discount
}
object Orange {
  val name = "Orange"
}

case class Discount (opLeftDiscount : Int, numDisc: Int, cost: BigDecimal)  {
  val discount: BigDecimal = numDisc / opLeftDiscount match {
    case res if res >= 1 => res * cost
    case _ => 0
  }
}





