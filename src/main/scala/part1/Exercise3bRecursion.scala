package part1

object Exercise3bRecursion {
  def contains(numbers: List[Int], num: Int): Boolean = {
    if (numbers.isEmpty) false
    else numbers.head == num || contains(numbers.tail, num)
  }

  def doubleEachNumber(numbers: List[Int]): List[Int] = {
    if (numbers.isEmpty) Nil
    else (numbers.head * 2) :: doubleEachNumber(numbers.tail)
  }

  def total(numbers: List[Int]): Int = {
    if (numbers.isEmpty) 0
    else numbers.head + total(numbers.tail)
  }

  def range(from: Int, to: Int): List[Int] = {
    if (from > to) Nil
    else from :: range(from + 1, to)
  }

  def append(a: List[Int], b: List[Int]): List[Int] = {
    if (a.isEmpty) b
    else a.head :: append(a.tail, b)
  }

  def main(args: Array[String]): Unit = {
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

    println("range")
    println(range(3, 5))

    println("append")
    // println(append(List(1, 2, 3), List(4, 5, 6)))
    // println(append(Nil, Nil))
  }
}
