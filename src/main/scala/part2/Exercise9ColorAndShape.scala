package part2

// ----------------------------------------------

// Write a definition for Color here!
case class Color(red: Int, green: Int, blue: Int)

// Write definitions for Shape, Circle, and Rect here!
sealed trait Shape {
  val color: Color
  def area(): Double = this match {
    case Circle(radius, _)      => radius * radius * math.Pi
    case Rect(width, length, _) => width * length
  }

  def perimeter(): Double = this match {
    case Circle(radius, _)      => 2 * math.Pi * radius
    case Rect(width, length, _) => 2 * (width + length)
  }
}
case class Circle(radius: Int, color: Color) extends Shape
case class Rect(width: Int, length: Int, override val color: Color) extends Shape

// ----------------------------------------------

object Exercise9ColorAndShape {

  val shape: Shape  = Circle(10, Color(0, 0, 0))
  val shape2: Shape = Rect(10, 10, Color(0, 0, 0))

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
