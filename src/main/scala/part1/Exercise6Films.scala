package part1

import films.{Director, Film}

object Exercise6Films {
  // Complete the following methods.
  // The exercises value gradually more complex as you go on.
  // The idea is to practice chaining methods together.
  // You DO NOT need to reference previous answers in later ones.

  def flip[A, B, C](f: A => (B => C)): (B => (A => C)) =
    (y: B) => (x: A) => f(x)(y)

  def nameOfFilm(film: Film): String = {
    film.name
  }

  def filmsByDirector(director: Director): List[Film] = {
    director.films
  }

  def directorsWithBackCatalogOfSize(
      directors: List[Director],
      numberOfFilms: Int
  ): List[Director] = {
    // directors.filter(hasMinFilms)(_)(numberOfFilms)
    directors.filter(hasMinFilms(numberOfFilms))
  }

  def directorsBornBefore(
      directors: List[Director],
      year: Int
  ): List[Director] = {
    directors.filter(bornBefore(year))
  }

  val hasMinFilms: Int => (Director => Boolean) =
    filmNumber => (director => director.films.length >= filmNumber)

  val bornBefore: Int => (Director => Boolean) = year =>
    director => director.yearOfBirth < year

  type Predicate[A] = A => Boolean

  def all[A](ps: Predicate[A]*): Predicate[A] = a => ps.forall(_(a))

  def any[A](ps: Predicate[A]*): Predicate[A] = a => ps.exists(_(a))

  def directorsBornBeforeWithBackCatalogOfSize(
      directors: List[Director],
      year: Int,
      numberOfFilms: Int
  ): List[Director] =
    //directors.filter(all(hasMinFilms(numberOfFilms), bornBefore(year)))
    directors.filter(all(hasMinFilms(numberOfFilms), bornBefore(year)))

  def directorsBornBeforeWithBackCatalogOfSize1(
      directors: List[Director],
      year: Int,
      numberOfFilms: Int
  ): List[Director] = {
    val bornBeforeWithBackCatalogOfSize: (Director => Boolean) = director =>
      hasMinFilms(numberOfFilms)(director) && bornBefore(year)(director)

    directors.filter(bornBeforeWithBackCatalogOfSize)
  }

  def directorsBornBeforeWithBackCatalogOfSize2(
      directors: List[Director],
      year: Int,
      numberOfFilms: Int
  ): List[Director] = {
    val withBackCatalog: Int => List[Director] => List[Director] =
      num => directors => directorsWithBackCatalogOfSize(directors, num)

    val withBornBefore: Int => List[Director] => List[Director] =
      year => directors => directorsBornBefore(directors, year)

    val withBackCatalogAndBornBefore: List[Director] => List[Director] =
      withBackCatalog(numberOfFilms) andThen withBornBefore(year)

    withBackCatalogAndBornBefore(directors)
  }

  def namesOfFilms(films: List[Film]): List[String] = {
    // map(f: Film => String)
    films.map(film => film.name)
  }

  def namesOfFilmsByDirector(director: Director): List[String] = {
    namesOfFilms(director.films)
  }

  def namesOfFilmsByDirectorScoringAtLeast(
      director: Director,
      imdbRating: Double
  ): List[String] = {
    director.films
      .filter(film => film.imdbRating >= imdbRating)
      .map(film => film.name)
  }

  def main(args: Array[String]): Unit = {
    import films.TestData._

    println("nameOfFilm")
    println(nameOfFilm(memento))
    println(nameOfFilm(darkKnight))

    println("filmsByDirector")
    println(filmsByDirector(nolan))
    println(filmsByDirector(eastwood))

    println("directorsWithBackCatalogOfSize")
    println(directorsWithBackCatalogOfSize(directors, 1))
    println(directorsWithBackCatalogOfSize(directors, 4))
    println(directorsWithBackCatalogOfSize(directors, 5))
    println(directorsWithBackCatalogOfSize(Nil, 1))

    println("directorsBornBefore")
    println(directorsBornBefore(directors, 1930))
    println(directorsBornBefore(directors, 1931))
    println(directorsBornBefore(directors, 1951))
    println(directorsBornBefore(directors, 1952))
    println(directorsBornBefore(Nil, 2000))

    println("directorsBornBeforeWithBackCatalogOfSize")
    println(directorsBornBeforeWithBackCatalogOfSize(directors, 1931, 5))
    println(directorsBornBeforeWithBackCatalogOfSize(directors, 1931, 6))
    println(directorsBornBeforeWithBackCatalogOfSize(directors, 1930, 5))
    println(directorsBornBeforeWithBackCatalogOfSize(Nil, 2000, 1))

    println("namesOfFilms")
    println(namesOfFilms(List(memento, darkKnight)))
    println(namesOfFilms(Nil))

    println("namesOfFilmsByDirector")
    println(namesOfFilmsByDirector(nolan))
    println(namesOfFilmsByDirector(eastwood))
    println(namesOfFilmsByDirector(someGuy))

    println("namesOfFilmsByDirectorScoringAtLeast")
    println(namesOfFilmsByDirectorScoringAtLeast(nolan, 8.8))
    println(namesOfFilmsByDirectorScoringAtLeast(nolan, 8.9))
    println(namesOfFilmsByDirectorScoringAtLeast(someGuy, 0.0))
  }
}
