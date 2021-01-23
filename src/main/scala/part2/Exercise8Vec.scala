package part2

// ----------------------------------------------

// Write your Vec class (and companion object) here!

case class Vec(x: Int, y: Int) {
  def length = math.sqrt((x * x).toDouble + (y * y).toDouble)

  def +(other: Vec) = Vec(x + other.x, y + other.y)

  def *(num: Int) = Vec(x * num, y * num)
}

object Vec {
  def zero = Vec(0, 0)
  def unitX = Vec(1, 0)
  def unitY = Vec(0, 1)

  def longest(a: Vec, b: Vec) = if (a.length >= b.length) a else b

  def longest(vectors: List[Vec]): Vec = vectors match {
    case Nil                         => zero
    case head :: next if next == Nil => head
    case head :: next if next != Nil => longest(head, longest(next))
  }
}

// ----------------------------------------------

object Exercise8Vec {
  // val vec1 = new Vec(3, 4)
  // val vec2 = new Vec(5, 12)

  def main(args: Array[String]): Unit = {
    println("length")
    // println(vec1.length)
    // println(vec2.length)

    println("+")
    // println(vec1 + vec2)
    // println((vec1 + vec2).length)

    println("*")
    // println(vec1 * 10)
    // println((vec1 * 10).length)

    println("zero, unitX, unitY")
    // println(Vec.zero)
    // println(Vec.unitX * 3 + Vec.unitY * 4)

    println("longest(Vec, Vec)")
    // println(Vec.longest(vec1, vec2))
    // println(Vec.longest(Vec.unitX * 2, Vec.unitY))

    println("longest(List[Vec])")
    // println(Vec.longest(List(Vec.unitX, Vec.unitX + Vec.unitY, Vec.unitY)))
    // println(Vec.longest(Nil))
  }
}
