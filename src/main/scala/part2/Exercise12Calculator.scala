package part2

// ----------------------------------------------

// Step 1. Write a definition for Expr here!

sealed abstract class Expr {
  def stringify: String =
    this match {
      case Num(value)       => value.toString
      case Add(left, right) => s"${left.stringify} + ${right.stringify}"
      case Mul(left, right) => s"${left.stringify} * ${right.stringify}"
      case Sub(left, right) => s"${left.stringify} - ${right.stringify}"
      case Div(left, right) => s"${left.stringify} / ${right.stringify}"
      case Sqrt(value)      => s"SQRT(${value.stringify})"
    }

}

case class Num(value: Double)           extends Expr // Base case
case class Add(left: Expr, right: Expr) extends Expr // Recursive case
case class Mul(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Sqrt(value: Expr)            extends Expr

// Handle the following types of equation:
// - addition
// - subtraction
// - multiplication
// - division
// - square root

// Give it a `stringify` method
// that renders the expression as a string.

// ----------------------------------------------

// Step 2. Implement eval
// for each of the "calculator" objects below:

object Calculator {
  def eval(calc: Expr): Double =
    calc match {
      case Num(value)       => value
      case Add(left, right) => eval(left) + eval(right)
      case Mul(left, right) => eval(left) * eval(right)
      case Sub(left, right) => eval(left) - eval(right)
      case Div(left, right) => eval(left) / eval(right)
      case Sqrt(value)      => math.sqrt(eval(value))
    }
}

object IntCalculator {
  def eval(calc: Expr): Int = calc match {
    case Num(value)       => value.toInt
    case Add(left, right) => eval(left) + eval(right)
    case Mul(left, right) => eval(left) * eval(right)
    case Sub(left, right) => eval(left) - eval(right)
    case Div(left, right) => eval(left) / eval(right)
    case Sqrt(value)      => math.sqrt(eval(value)).toInt
  }
}

// ----------------------------------------------

// Step 3. Write some convenience methods
// for constructing common calculations:

// ----------------------------------------------

object Expr {
  // c = sqrt ( a2 + b2 )
  def pythag(a: Double, b: Double): Expr =
    Sqrt(Add(Mul(Num(a), Num(a)), Mul(Num(b), Num(b))))
    
  def factorial(n: Int): Expr = {
    if (n==1) Num(1) else Mul(Num(n), factorial(n-1))
  }
}

object Exercise11Calculator {
  // val calc1 = Add(Num(1.1), Mul(Num(2.2), Num(3.3)))
  // val calc2 = Add(Mul(Num(1.1), Num(2.2)), Num(3.3))

  def main(args: Array[String]): Unit = {
    println("stringify")
    // println(calc1.stringify)
    // println(calc2.stringify)

    println("Calculator.eval")
    // println(Calculator.eval(calc1))
    // println(Calculator.eval(calc2))

    println("IntCalculator.eval")
    // println(IntCalculator.eval(calc1))
    // println(IntCalculator.eval(calc2))

    println("pythag")
    // println(Expr.pythag(3, 4))
    // println(Calculator.eval(Expr.pythag(3, 4)))
    // println(IntCalculator.eval(Expr.pythag(3, 4)))

    println("factorial")
    // println(Expr.factorial(4))
    // println(Calculator.eval(Expr.factorial(4)))
    // println(IntCalculator.eval(Expr.factorial(4)))
  }
}
