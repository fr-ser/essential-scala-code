package part1

object Exercise3aRecursion {
  def greetNTimes(name: String, n: Int): Unit =
    if (n <= 0) ()
    else {
      println(name)
      greetNTimes(name, n - 1)
    }

  def main(args: Array[String]): Unit = {
    println("greetNTimes")
    greetNTimes("world", 5)
    greetNTimes("nope", 0)
  }
}
