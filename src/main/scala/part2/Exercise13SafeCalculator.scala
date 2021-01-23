package part2

// ----------------------------------------------

// Step 1. Implement a `Result` type

sealed abstract class Result
case class Pass(num: Double) extends Result
case class Fail(reason: String) extends Result
// A Result is:
//   - a Pass containing a Double result, or
//   - a Fail containing a String error message

// ----------------------------------------------

sealed abstract class Expr {
  def stringify: String

}
case class Num(a: Double) extends Expr {
  def stringify: String = a.toString
}
case class Add(a: Expr, b: Expr) extends Expr {
  def stringify: String = s"${a.stringify} + ${b.stringify}"

}
case class Sub(a: Expr, b: Expr) extends Expr {
  def stringify: String = s"${a.stringify} - ${b.stringify}"

}
case class Mul(a: Expr, b: Expr) extends Expr {
  def stringify: String = s"${a.stringify} * ${b.stringify}"

}
case class Div(a: Expr, b: Expr) extends Expr {
  def stringify: String = s"${a.stringify} / ${b.stringify}"

}
case class Sqrt(a: Expr) extends Expr {
  def stringify: String = s"sqrt(${a.stringify})"

}

// Step 2. Implement eval below:

object SafeCalculator {
  private def passEval(a: Expr, b: Expr, fn: (Double, Double) => Double) =
    eval(a) match {
      case f: Fail => f
      case Pass(aVal) =>
        eval(b) match {
          case f: Fail    => f
          case Pass(bVal) => Pass(fn(aVal, bVal))
        }
    }

  def eval(calc: Expr): Result = calc match {
    case Num(a)    => Pass(a)
    case Add(a, b) => passEval(a, b, (x, y) => x + y)
    case Sub(a, b) => passEval(a, b, (x, y) => x - y)
    case Mul(a, b) => passEval(a, b, (x, y) => x * y)

    case Div(a, b) =>
      eval(a) match {
        case f: Fail => f
        case Pass(aVal) =>
          eval(b) match {
            case f: Fail    => f
            case Pass(0)    => Fail("Division by zero")
            case Pass(bVal) => Pass(aVal * bVal)
          }
      }
    case Sqrt(a) =>
      eval(a) match {
        case f: Fail => f
        case Pass(aVal) =>
          if (aVal >= 0) Pass(math.sqrt(aVal))
          else Fail("Square root of negative number")
      }
  }
}

object Exercise13SafeCalculator {
  def main(args: Array[String]): Unit = {
    println("SaveCalculator.eval")
    // println(SafeCalculator.eval(Add(Num(1), Num(2))))
    // println(SafeCalculator.eval(Sqrt(Num(-1))))
    // println(SafeCalculator.eval(Div(Num(1), Num(0))))
  }
}
