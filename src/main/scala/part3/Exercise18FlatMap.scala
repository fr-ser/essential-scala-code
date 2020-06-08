package part3

import films.{Director, Film}

object Exercise18FlatMap {
  def filmsByDirector(director: Director): List[Film] =
    director.films

  def namesOfFilmsByDirector(director: Director): List[String] =
    filmsByDirector(director).map(_.name)

  def filmsByAllDirectors(directors: List[Director]): List[Film] =
    directors.flatMap(_.films)

  def namesOfFilmsByAllDirectors(directors: List[Director]): List[String] =
    // filmsByAllDirectors(directors).map(_.name)
    directors.flatMap(_.films.map(_.name))

  // Return a list of messages of the form "Tonight only! <FILM> by <DIRECTOR>!"
  def tonightOnlyMessages(directors: List[Director]): List[String] =
    directors.flatMap(d => d.films.map(f => s"Tonight only! ${f.name} by ${d.firstName} ${d.lastName}!"))

  def main(args: Array[String]): Unit = {
    import films.TestData._

    println("filmsByDirector")
    // println(filmsByDirector(nolan))
  }
}