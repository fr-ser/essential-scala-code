package part1

object Exercise4PatternMatching {
  def greetNTimes(name: String, n: Int): Unit = {
    ???
  }

  def contains(numbers: List[Int], num: Int): Boolean = {
    numbers match {
      case Nil          => false
      case head :: next => head == num || contains(next, num)
    }
  }

  def doubleEachNumber(numbers: List[Int]): List[Int] = {
    numbers match {
      case Nil          => Nil
      case head :: next => head * 2 :: doubleEachNumber(next)
    }
  }

  def total(numbers: List[Int]): Int = {
    numbers match {
      case Nil          => 0
      case head :: next => head + total(next)
    }
  }

  def append(a: List[Int], b: List[Int]): List[Int] = {
    a match {
      case Nil          => b
      case head :: next => head :: append(next, b)
    }
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
