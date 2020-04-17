package part2

// ----------------------------------------------

// Write your Counter class (and companion object) here!
class Counter(val value: Int) {
  def increment = new Counter(value + 1)

  private val foo = ""

  override def toString(): String = s"Counter($value)"
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[Counter]) {
      obj.asInstanceOf[Counter].value == value
    } else false
}

object Counter {
  def apply(value: Int): Counter = new Counter(value)
  def apply(): Counter = new Counter(0)

  def foo(c: Counter) = c.foo
}
// ----------------------------------------------

object Example7Counter {
  def main(args: Array[String]): Unit = {
    println("constructors")
    val counter1 = new Counter(10)
    val counter2 = new Counter(20)
    println(counter1.value)
    println(counter2.value)

    println("increment")
    val counter3 = new Counter(41)
    val counter4 = counter3.increment
    println(counter3.value)
    println(counter4.value)

    println("toString")
    println(counter1)
    println(counter4)

    println("equality")
    val counter5 = new Counter(100)
    val counter6 = new Counter(100)
    println(counter5 == counter6)

    println("companion object")

    println(Counter().increment.increment)
    // println(Counter.zero.increment.increment.increment)
  }
}
