package part1

//exists(p: A => Boolean): Boolean
object Exercise5Functions {
  def contains(numbers: List[Int], num: Int): Boolean = {
    val predicate: Int => Boolean = (x: Int) => x == num // function, first-order function
    numbers.exists(
      predicate
    ) // existis: function, higher-order function takes function as parameter
  }

  // def exists[A](xs: List[A], p: A => Boolean): Boolean =
  //   xs match {
  //     case Nil     => false
  //     case x :: xs => p(x) || exists(xs, p)
  //   }

  // def contains(numbers: List[Int], num: Int): Boolean = numbers match {
  //   case Nil          => false
  //   case head :: tail => head == num || contains(tail, num)
  // }

  def containsEvenNumbers(numbers: List[Int]): Boolean = {
    val predicate: Int => Boolean = (x: Int) => x % 2 == 0 // First-order function, it only contains ordinary input types
    numbers.exists(
      predicate
    ) // higher-order function, because it takes a function as a parameter
  }

  def evenNumbersOnly(numbers: List[Int]): List[Int] = {
    // val predicate: Int => Boolean = (x: Int) => x % 2 == 0
    numbers.filter(x => x % 2 == 0)
    // numbers.filter(_ % 2 == 0)
  }

  def doubleEachNumber(numbers: List[Int]): List[Int] = {
    val timesTwo: Int => Int = _ * 2
    //val timesTwoUgly = (_: Int) * 2 // ugly
    numbers.map(timesTwo)
    // x => x <=> _
    //numbers.map(_ * 2)
    //map[B](f: Int => B): List[B]
    // map[B](f: Int => Int): List[B]
  }

  def multiplyEachNumberBy(numbers: List[Int], num: Int): List[Int] =
    numbers.map(_ * num)

  def evenNumbersOnlyDoubled(numbers: List[Int]): List[Int] = {
    val f = (evenNumbersOnly _) andThen doubleEachNumber
    val f2 = (doubleEachNumber _) compose evenNumbersOnly

    f(numbers)
    // val isEven = (x: Int) => x % 2 == 0
    // numbers.filter(isEven).map(_ * 2)
  }

  def fof(a: Int) = ???
  def hof(f: Int => Int) = ???
  def hof2(a: Int): (Int => Int) = ???
  /*
  https://en.wikipedia.org/wiki/Function_composition
In mathematics, function composition is an operation that takes two functions f and g and produces a function h such that h(x) = g(f(x)). In this operation, the function g is applied to the result of applying the function f to x. That is, the functions f : X → Y and g : Y → Z are composed to yield a function that maps x in X to g(f(x)) in Z.

Intuitively, if z is a function of y, and y is a function of x, then z is a function of x. The resulting composite function is denoted g ∘ f : X → Z, defined by (g ∘ f )(x) = g(f(x)) for all x in X.[note 1] The notation g ∘ f is read as "g circle f ", "g round f ", "g about f ", "g composed with f ", "g after f ", "g following f ", "g of f", or "g on f ". Intuitively, composing functions is a chaining process in which the output of function f feeds the input of function g.

The composition of functions is a special case of the composition of relations, so all properties of the latter are true of composition of functions.[1] The composition of functions has some additional properties.
   */
  // h = f compose g => h(x) = f(g(x))
  def compose(f: Int => Int, g: Int => Int): (Int => Int) = x => f(g(x))
  def andThen(f: Int => Int, g: Int => Int): (Int => Int) = x => g(f(x))

  // compose / andthen

  // applyTwice

  // curry / uncurry

  // flip

  // const

  def main(args: Array[String]): Unit = {
    println("contains")
    // println(contains(List(1, 2, 3), 2))
    // println(contains(List(1, 2, 3), 4))
    // println(contains(Nil, 1))

    println("containsEvenNumbers")
    // println(containsEvenNumbers(List(1, 2, 3)))
    // println(containsEvenNumbers(List(1, 3, 5)))
    // println(containsEvenNumbers(Nil))

    println("evenNumbersOnly")
    // println(evenNumbersOnly(List(1, 2, 3, 4)))
    // println(evenNumbersOnly(Nil))

    println("doubleEachNumber")
    // println(doubleEachNumber(List(1, 2, 3, 4)))
    // println(doubleEachNumber(Nil))

    println("multiplyEachNumberBy")
    // println(multiplyEachNumberBy(List(1, 2, 3, 4), 5))
    // println(multiplyEachNumberBy(List(1, 2, 3, 4), 10))
    // println(multiplyEachNumberBy(Nil, 100))

    println("evenNumbersOnlyDoubled")
    // println(evenNumbersOnlyDoubled(List(1, 2, 3, 4)))
    // println(evenNumbersOnlyDoubled(Nil))
  }
}
