package part2

// ----------------------------------------------

// Step 1. Implement a `Result` type

sealed abstract class Result[A] // Either
{
  def map[B](f: A => B): Result[B] =
    this match {
      case Fail(error) => Fail(error)
      case Pass(value) => Pass(f(value))
    }

  def flatMap[B](f: A => Result[B]): Result[B] =
    this match {
      case Pass(value /* : A */ ) => f(value)
      case Fail(error)            => Fail(error)
    }
}
case class Pass[A](value: A)      extends Result[A] // Right
case class Fail[A](error: String) extends Result[A] // Left

// A Result is:
//   - a Pass containing a Double result, or
//   - a Fail containing a String error message

// ----------------------------------------------

object SafeCalculator {
  def eval(calc: Expr): Result[Double] =
    calc match {
      case Num(value) => Pass(value)
      case Add(left, right) => 
        for {
          l <- eval(left) 
          r <- eval(right)
        } yield l + r 
      case Mul(left, right) => eval(left).flatMap(l => eval(right).map(r => l * r))
      case Sub(left, right) => eval(left).flatMap(l => eval(right).map(r => l - r))
      case Div(left, right) =>
        eval(left).flatMap(l =>
          eval(right).flatMap {
            case 0 => Fail("Can't divide by zero")
            case r => Pass(l / r)
          }
        )

      case Sqrt(value) =>
        eval(value).flatMap {
          case value if (value < 0) => Fail("Can't calculate square root of negative number")
          case value                => Pass(math.sqrt(value))
        }

    }
}
// Step 2. Implement eval below:

object Exercise13SafeCalculator {
  def main(args: Array[String]): Unit =
    println("SaveCalculator.eval")
  // println(SafeCalculator.eval(Add(Num(1), Num(2))))
  // println(SafeCalculator.eval(Sqrt(Num(-1))))
  // println(SafeCalculator.eval(Div(Num(1), Num(0))))
}
