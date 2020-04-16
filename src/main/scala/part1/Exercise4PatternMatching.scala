package part1

object Exercise4PatternMatching {
  def greetNTimes(name: String, n: Int): Unit = n match {
    case 0 => ()
    case i =>
      println(name)
      greetNTimes(name, i - 1)
  }

  // sealed trait List[A]
  // case object Nil                                extends List[Nothing]
  // final case class ::[A](head: A, tail: List[A]) extends List[A]

  def contains(numbers: List[Int], num: Int): Boolean = numbers match {
    case Nil          => false
    case head :: tail => head == num || contains(tail, num)
    // case x :: xs => x == num || (xs contains x)
  }

  def doubleEachNumber(numbers: List[Int]): List[Int] = numbers match {
    case Nil          => Nil
    case head :: tail => head * 2 :: doubleEachNumber(tail)
  }

  def total(numbers: List[Int]): Int = numbers match {
    case Nil          => 0
    case head :: tail => head + total(tail)
  }

  def append(first: List[Int], second: List[Int]): List[Int] =
    first match {
      case Nil          => second
      case head :: tail => head :: append(tail, second)
    }

  def main(args: Array[String]): Unit = {
    println("greetNTimes")
    // println(greetNTimes("world", 5))
    // println(greetNTimes("nope", 0))

    println("contains")
    // println(contains(List(1, 2, 3), 2))
    // println(contains(List(1, 2, 3), 4))
    // println(contains(Nil, 1))

    println("doubleEachNumber")
    // println(doubleEachNumber(List(1, 2, 3, 4)))
    // println(doubleEachNumber(Nil))

    println("total")
    // println(total(List(1, 2, 3, 4)))
    // println(total(Nil))

    println("append")
    // println(append(List(1, 2, 3), List(4, 5, 6)))
    // println(append(Nil, Nil))
  }
}
