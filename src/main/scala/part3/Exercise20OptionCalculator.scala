package part3

import part2._

object OptionCalculator {
  private def passEval(a: Expr, b: Expr, fn: (Double, Double) => Double) =
    eval(a) match {
      case None => None
      case Some(aVal) =>
        eval(b) match {
          case None       => None
          case Some(bVal) => Some(fn(aVal, bVal))
        }
    }

  def eval(calc: Expr): Option[Double] = {
    calc match {
      case Num(a)    => Some(a)
      case Add(a, b) => passEval(a, b, (x, y) => x + y)
      case Sub(a, b) => passEval(a, b, (x, y) => x - y)
      case Mul(a, b) => passEval(a, b, (x, y) => x * y)

      case Div(a, b) =>
        eval(a) match {
          case None => None
          case Some(aVal) =>
            eval(b) match {
              case None       => None
              case Some(0)    => None
              case Some(bVal) => Some(aVal * bVal)
            }
        }
      case Sqrt(a) =>
        eval(a) match {
          case None => None
          case Some(aVal) =>
            if (aVal >= 0) Some(math.sqrt(aVal))
            else None
        }
    }
  }
}

object Exercise20OptionCalculator {
  def main(args: Array[String]): Unit = {
    println("OptionCalculator.eval")
    // println(OptionCalculator.eval(Add(Num(1), Num(2))))
    // println(OptionCalculator.eval(Sqrt(Num(-1))))
    // println(OptionCalculator.eval(Div(Num(1), Num(0))))
  }
}
