package part2

// ----------------------------------------------

// Write your Vec class (and companion object) here!

// ----------------------------------------------

case class Vec(x: Double, y: Double) { 
  def length: Double = Math.sqrt(x * x + y * y)

  def +(vec: Vec): Vec = new Vec (this.x + vec.x, this.y + vec.y)

  def *(multiplier: Int): Vec =
    new Vec(this.x * multiplier, this.y * multiplier)

}
object Vec {

  def zero: Vec = new Vec(0, 0)
  def unitX: Vec = new Vec(1, 0)
  def unitY: Vec = new Vec(0, 1)
  def longest(vec1: Vec, vec2: Vec): Vec =
    if (vec1.length < vec2.length) vec2 else vec1

  def longest(vecs: List[Vec]): Vec = vecs match {
    case Nil          => Vec.zero
    case head :: Nil  => head
    case head :: tail => longest(head, longest(tail))
  }
}

class Vec1(val x: Double, val y: Double) {

  def length: Double = Math.sqrt(x * x + y * y)

  def +(vec: Vec): Vec = new Vec (this.x + vec.x, this.y + vec.y)

  def *(multiplier: Int): Vec =
    new Vec(this.x * multiplier, this.y * multiplier)
    
  override def toString(): String = s"Vec($x,$y)"
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[Vec1]) {
      obj.asInstanceOf[Vec1].x == x && obj.asInstanceOf[Vec1].y == y
    } else false

  def copy(x: Double = this.x, y: Double = this.y) = Vec1(x, y)

}

object Vec1 {
  def apply(x: Double, y: Double): Vec1 = new Vec1(x, y)
  def unapply(vec: Vec1): Option[(Double, Double)] = {
    val tuple = (vec.x, vec.y)
    Some(tuple)
  }

  def zero: Vec1 = new Vec1(0, 0)
  def unitX: Vec1 = new Vec1(1, 0)
  def unitY: Vec1 = new Vec1(0, 1)
  def longest(vec1: Vec1, vec2: Vec1): Vec1 =
    if (vec1.length < vec2.length) vec2 else vec1

  def longest(vecs: List[Vec1]): Vec1 = vecs match {
    case Nil          => Vec1.zero
    case head :: Nil  => head
    case head :: tail => longest(head, longest(tail))
  }

  import Ordering.Double.TotalOrdering

  // implicit var vecOrdering : Ordering[Vec] = new Ordering[Vec]{
  //     def compare(x: Vec, y: Vec): Int =
  //       if (x.length < y.length) -1
  //       else if (x.length > y.length) 1
  //       else 0
  // }
  // def maxBy[B](f: Vec => B)(implicit cmp: Ordering[B]): Vec
  def longest2(vecList: List[Vec1]): Vec1 =
    if (vecList.isEmpty) Vec1.zero else vecList.maxBy(eachVec => eachVec.length)

  implicit var vecOrdering: Ordering[Vec1] =
    Ordering.by((vec: Vec1) => vec.length)
  def longestMax(vecList: List[Vec1]): Vec1 =
    if (vecList.isEmpty) Vec1.zero else vecList.max
}

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
