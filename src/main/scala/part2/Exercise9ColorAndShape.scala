package part2

// ----------------------------------------------

// Write a definition for Color here!

case class Color(r: Int, g: Int, b: Int)

// Write definitions for Shape, Circle, and Rect here!

sealed trait Shape {
  def area = this match {
    case Circle(r, _)  => r * r * math.Pi
    case Rect(l, w, _) => (l * w).toDouble
  }

  def perimeter = this match {
    case Circle(r, _)  => 2 * r * math.Pi
    case Rect(l, w, _) => 2.0 * (l + w)
  }

  def color: Color
}
case class Circle(r: Int, override val color: Color) extends Shape
case class Rect(l: Int, w: Int, override val color: Color) extends Shape

// ----------------------------------------------

object Exercise9ColorAndShape {
  def main(args: Array[String]): Unit = {
    println("color")
    // println(Color(1, 1, 0))
    // println(Color(1, 0, 1))

    println("circle")
    // val circle = Circle(30, Color(1, 1, 0))
    // println(circle)
    // println(circle.area)
    // println(circle.perimeter)

    println("rect")
    // val rect = Rect(50, 30, Color(1, 0, 1))
    // println(rect)
    // println(rect.area)
    // println(rect.perimeter)
  }
}
