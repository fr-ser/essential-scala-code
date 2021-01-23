package part3

import films.{Director, Film}

object Exercise22Sorting {
  def filmsSortedByImdb(films: List[Film]): List[Film] = {
    films.sortBy(_.imdbRating)(Ordering.Double.IeeeOrdering)
  }

  def filmsByDirectorSortedByImdb(director: Director): List[Film] = {
    director.films.sortBy(_.imdbRating)(Ordering.Double.IeeeOrdering)
  }

  def filmsByAllDirectorsSortedByImdb(directors: List[Director]): List[Film] = {
    directors
      .flatMap(_.films)
      .sortBy(_.imdbRating)(Ordering.Double.IeeeOrdering)
  }

  def filmsByAllDirectorsSortedByDirectorNameThenImdb(
      directors: List[Director]
  ): List[Film] = {
    directors
      .sortBy(_.lastName)
      .reverse
      .flatMap((d) =>
        d.films.sortBy(_.imdbRating)(Ordering.Double.IeeeOrdering)
      )
  }

  def averageImdbRating(films: List[Film]): Double = {
    films.foldLeft(0.0)((acc, film) => acc + film.imdbRating) / films.length
  }

  def averageImdbRatingAcrossDirectors(directors: List[Director]): Double = {
    directors
      .flatMap(_.films)
      .foldLeft(0.0)((acc, film) => acc + film.imdbRating) / directors
      .flatMap(_.films)
      .length
  }

  def main(args: Array[String]): Unit = {
    import films.TestData._

    println("filmsSortedByImdb")
    // println(filmsSortedByImdb(mcTiernan.films))
    // println(filmsSortedByImdb(eastwood.films))

    println("filmsByDirectorSortedByImdb")
    // println(filmsByDirectorSortedByImdb(mcTiernan))
    // println(filmsByDirectorSortedByImdb(eastwood))

    println("filmsByAllDirectorsSortedByImdb")
    // println(filmsByAllDirectorsSortedByImdb(List(nolan, mcTiernan)))

    println("filmsByAllDirectorsSortedByDirectorNameThenImdb")
    println(
      filmsByAllDirectorsSortedByDirectorNameThenImdb(List(nolan, mcTiernan))
    )

    println("averageImdbRating")
    // println(averageImdbRating(nolan.films))
    // println(averageImdbRating(someGuy.films))

    println("averageImdbRatingAcrossDirectors")
    // println(averageImdbRatingAcrossDirectors(directors))
  }
}
