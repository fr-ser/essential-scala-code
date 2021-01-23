package part1

object Exercise2Expressions {
  def greet(name: String): Unit = {
    ???
  }

  def greeting(name: String): String = s"Hello $name!"

  // Hint - You can reverse a string with `string.reverse`
  def palindrome(str: String): Boolean = str == str.reverse

  def factorial(n: Int): Int = if (n < 2) n else n * factorial(n - 1)

  def main(args: Array[String]): Unit = {
    println("greet")
    // println(greet("Earthlings"))

    println("palindrome")
    // println(palindrome("taco"))

    println("factorial")
    // println(factorial(10))
  }
}
