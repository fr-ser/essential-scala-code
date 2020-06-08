import $ivy.`org.typelevel::cats-core:2.2.0-M1`
import cats.implicits._

val numbers = List(1, 2, 3)
val letters = List("a", "b", "c")
val symbols = List("?", "!")

val result: List[String] = numbers.flatMap(i => letters.flatMap(l => symbols.map(s => s"$i-$l-$s")))

val result2 = for {
  i: Int    <- numbers: List[Int]
  l: String <- letters: List[String]
  s: String <- symbols: List[String]
} yield s"$i-$l-$s".toUpperCase()

println(result)
println(result2)
