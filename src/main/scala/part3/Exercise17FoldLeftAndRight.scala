package part3

import films.Film

object Exercise17FoldLeftAndRight {
  def totalImdbRating(films: List[Film]): Double = {
    films.foldLeft(0.0)((acc, film) => acc + film.imdbRating)
  }

  def averageImdbRating(films: List[Film]): Double = {
    films.foldLeft(0.0)((acc, film) => acc + film.imdbRating) / films.length
  }

  def reverseUsingFold[A](items: List[A]): List[A] = {
    items.foldLeft[List[A]](Nil)((acc, item) => item :: acc)
  }

  def filterUsingFold[A](items: List[A], pred: A => Boolean): List[A] = {
    items.foldLeft[List[A]](Nil)((acc, item) =>
      if (pred(item)) acc :+ item else acc
    )
  }

  def main(args: Array[String]): Unit = {
    import films.TestData._

    println("totalImdbRating")
    // println(totalImdbRating(List(memento, outlawJoseyWales, thomasCrownAffair)))
    // println(totalImdbRating(Nil))

    println("averageImdbRating")
    // println(averageImdbRating(List(memento, outlawJoseyWales, thomasCrownAffair)))
    // println(averageImdbRating(Nil))

    println("reverseUsingFold")
    // println(reverseUsingFold(List(1, 2, 3)))
    // println(reverseUsingFold(Nil))

    println("filterUsingFold")
    // println(filterUsingFold(List(1, 2, 3, 4, 5), (n: Int) => n % 2 == 0))
    // println(filterUsingFold(Nil, (n: Int) => n % 2 == 0))
  }
}
