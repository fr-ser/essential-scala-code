package part2

// ----------------------------------------------

// Step 1. Implement MyList[A] below
// Any 
// A 
// Nothing 

sealed abstract class  MyList2[+A] // covariant B <: A => MyList[B] <: MyList[A]
                    // MyList[A]  // invariance B <: A => 
                    // MyList[-A]  // contravariance B <: A => MyList[B] :> MyList[A]
case class MyPair2[A](head:A, tail:MyList2[A]) extends MyList2[A]
case object MyNil2 extends MyList2[Nothing] 

sealed abstract class  MyList3[-A] // covariant B <: A => MyList[B] <: MyList[A]
                    // MyList[A]  // invariance B <: A => 
                    // MyList[-A]  // contravariance B <: A => MyList[B] :> MyList[A]
case class MyPair3[A](head:A, tail:MyList3[A]) extends MyList3[A]
case object MyNil3 extends MyList3[Any] 

// Nothing <: A  
// MyList[Nothing] <: MyList[A]
object Test {
  val x : MyList2[Int] = MyNil2 : MyList2[Nothing] // MyList2[Nothing] <: MyList2[Int] - covariance 
  val y : MyList3[Int] = MyNil3 : MyList3[Any] // MyList3[Any] <: MyList3[Int] - contravariance
}

sealed abstract class MyList[A] {
  def exists2(predicate: A => Boolean): Boolean =
    this match {
      case MyNil()            => false
      case MyPair(head, tail) => predicate(head) || tail.exists(predicate)
    }

  // List(1,2,3) => ( (false || predicate(1) ) || predicate(2) )  || predicate(3)
  def exists(predicate: A => Boolean): Boolean = this.reduce2(false)((accum: Boolean, element: A) => accum || predicate(element))

  def contains(elem: A): Boolean = this.exists(_ == elem)

  def map[B](func: A => B): MyList[B] =
    this match {
      case MyNil()            => MyNil()
      case MyPair(head, tail) => MyPair(func(head), tail.map(func))
    }

  def filter(predicate: A => Boolean): MyList[A] =
    this match {
      case MyNil() => MyNil()
      case MyPair(head, tail) =>
        if (predicate(head)) MyPair(head, tail.filter(predicate))
        else tail.filter(predicate)
    }

  def reduce(empty: A, func: (A, A) => A): A =
    this match {
      case MyNil()            => empty
      case MyPair(head, tail) => tail.reduce(func(empty, head), func)
    }

  def reduce2[B](empty: B)(func: (B, A) => B): B =
    this match {
      case MyNil()            => empty
      case MyPair(head, tail) => tail.reduce2(func(empty, head))(func)
    }

  def append(list: MyList[A]): MyList[A] =
    this match {
      case MyNil()            => list
      case MyPair(head, tail) => MyPair(head, tail.append(list))
    }

  // flatten : MyList[MyList[A]] => MyList[A]
  // flatMap = nestedList // MyList[A]
  //              .map(func) // MyList[MyList[A]]
  //              .flatten // MyList[A]

  def flatMap[B](func: A => MyList[B]): MyList[B] =
    this match {
      case MyNil()            => MyNil()
      case MyPair(head, tail) => func(head).append(tail.flatMap(func))
    }

}
case class MyPair[A](head: A, tail: MyList[A]) extends MyList[A]
case class MyNil[A]()                          extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] = as.toList.foldRight(MyNil(): MyList[A])((a, acc) => MyPair(a, acc))
}

// Step 2. Implement the following methods
// using methods from IntList as templates:

// - exists(func)
//   Does the list contain a particular value?
//   Use `contains` as a template

// - map(func)
//   Applies `func` to each item, returning a list of the results
//   Use `addToEach` as a template

// Harder methods:

// - filter(predicate)
//   Applies `predicate` to each item to get a boolean result
//   Return a new list, but only include items for which `predicate` returns true
//   Use `evensOnly` as a template

// - reduce(accum, func)
//   Start with `accum`. Use `func` to combine it with the head of the list.
//   Then use the result as the `accum` to reduce the tail of the list.
//   Use `total` as a template

// - append(list)
//   Append two lists

// ----------------------------------------------

object Exercise14GenericList {
  // val numbers: MyList[Int] =
  //   MyPair(1, MyPair(3, MyPair(5, MyNil())))

  // val strings: MyList[String] =
  //   MyPair("foo", MyPair("bar", MyPair("baz", MyNil())))

  // val shapes: MyList[Shape] =
  //   MyPair(
  //     Circle(20, Color(1, 1, 0)),
  //     MyPair(
  //       Circle(10, Color(1, 1, 0)),
  //       MyPair(
  //         Rect(30, 20, Color(1, 0, 1)),
  //         MyNil())))

  println("exists")
  // println(numbers.exists(n => n > 1))
  // println(strings.exists(s => s.startsWith("b")))
  // println(shapes.exists(s => s.area > 100))

  println("map")
  // println(numbers.map(n => n + 1))
  // println(strings.map(s => s + "!"))
  // println(shapes.map(s => s.toString))

  println("reduce")
  // println(numbers.reduce(0, (a, b) => a + b))
  // println(strings.reduce("", (a, b) => a + b))

  println("append")
  // println(numbers.append(numbers))
  // println(strings.append(strings))
  // println(shapes.append(shapes))

  println("filter")
  // println(numbers.filter(n => n > 1))
  // println(strings.filter(s => s.startsWith("b")))
  // println(shapes.filter(s => s.area > 50))
}
