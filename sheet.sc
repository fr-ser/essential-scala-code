import $ivy.`org.typelevel::cats-core:2.2.0-M1`
import cats.implicits._

val timesTwo = (x: Int) => x * 2
val increment = (x: Int) => x + 1

def compose(f: Int => Int, g: Int => Int): (Int => Int) = x => f(g(x))

def andThen(f: Int => Int, g: Int => Int): (Int => Int) = x => g(f(x))

def applyTwice(f: Int => Int): (Int => Int) = x => f(f(x))
val applyTwice2 = f => compose(f, f)

val addTwo = applyTwice2(increment)

def add(x: Int, y: Int): Int = x + y

def add_(x: Int): Int => Int = (y: Int) => x + y

val addUncurried: (Int, Int) => Int = (x: Int, y: Int) => { x + y }
val addCurried: Int => (Int => Int) = (x: Int) => { (y: Int) => x + y }

// addUncurried(3,2) ===curry===> addCurried(3)(2)
def curry(uncurried: (Int, Int) => Int): (Int => (Int => Int)) =
  x => y => uncurried(x, y)

// addCurried(3)(2) ===uncurry===> addUncurried(3,2)
def uncurry(curried: Int => (Int => Int)): (Int, Int) => Int =
  (x, y) => curried(x)(y)

val addCurried2 = curry(addUncurried)
val addUncurried2 = uncurry(addCurried)

println(addCurried2(10)(2))
println(addUncurried2(10, 2))

// Uncurried
List(1, 2, 3).map(y => addUncurried(5, y)) // add 5 to each element

// Curried
List(1, 2, 3).map(y => addCurried(5)(y)) /// add 5 to each element
List(1, 2, 3).map(addCurried(5)(_))

val add3 = (x: Int, y: Int, z: Int) => x + y + z

val result = add3(1, 2, 3)

val add3Curried = (x: Int) => (y: Int) => (z: Int) => x + y + z

val result2 = add3Curried(1)(2)(3)

def flip(f: Int => (String => String)): (String => (Int => String)) = {
  val flipped = (y: String) => (x: Int) => f(x)(y)
  flipped
}

val toUpperIfGreaterTen = (x: Int) =>
  (s: String) => if (x > 10) s.toUpperCase() else s

println(toUpperIfGreaterTen(11)("Hello World!"))

val flipped = flip(toUpperIfGreaterTen)

println(flipped("Hello World!")(11))

val addSix: Int => Int = addCurried(5) andThen increment

println(List(1, 2, 3).map(addSix))
