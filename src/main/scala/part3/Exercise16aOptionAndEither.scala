package part3

import films.{Director, Film}

object Exercise16aOptionAndEither {
  def directorWithLastName(directors: List[Director], lastName: String): Option[Director] =
    directors.find(_.lastName == lastName)

  def directorWithLastNameOrFailure(directors: List[Director], lastName: String): Either[String, Director] =
    directorWithLastName(directors, lastName) match {
      case Some(value) => Right(value)
      case None        => Left("404 not found")
    }

  def yearOfBirthOfDirectorWithLastName(directors: List[Director], lastName: String): Option[Int] =
    directorWithLastName(directors, lastName).map(_.yearOfBirth)

  def yearOfBirthOfDirectorWithLastNameOrFailure(directors: List[Director], lastName: String): Either[String, Int] =
    yearOfBirthOfDirectorWithLastName(directors, lastName) match {
      case Some(value) => Right(value)
      case None        => Left("nope")
    }

  def filmsByDirectorWithLastName(directors: List[Director], lastName: String): List[Film] =
    directorWithLastName(directors, lastName).map(_.films).getOrElse(Nil)

  def earliestFilmByDirectorWithLastName(
    directors: List[Director],
    lastName: String
  ): Option[Film] =
    filmsByDirectorWithLastName(directors, lastName)
      .sortBy(f => f.yearOfRelease)
      .headOption
// Solution 1:  
      // filmsByDirectorWithLastName(directors, lastName).minByOption(_.yearOfRelease)
  // Solution 2:
  // filmsByDirectorWithLastName(directors, lastName).foldLeft(None: Option[Film])((accum, film) =>
  //   accum.filter(_.yearOfRelease < film.yearOfRelease).orElse(Some(film))

  def earliestFilmByDirectorWithLastNameOrFailure(
    directors: List[Director],
    lastName: String
  ): Either[String, Film] =
    // Option(1).map(_ + 1) -> Option(2)
    // val x1 : Either[Int,Int] = Left(10)
    // val x2 : Either[Int,Int] = Right(5)
    // x1.map(_ + 1) == Left(10)
    // x2.map(_ + 1) == Right(6)  // right-biased
    earliestFilmByDirectorWithLastName(directors, lastName) //Option[Film]
      .toRight("Could not find film")                       //Either[String, Film]

  def namesOfFilmsByDirectorWithLastName(
    directors: List[Director],
    lastName: String
  ): List[String] =
    filmsByDirectorWithLastName(directors, lastName)
      .map(_.name)

  def nameOfEarliestFilmByDirectorWithLastName(
    directors: List[Director],
    lastName: String
  ): Option[String] =
    earliestFilmByDirectorWithLastName(directors, lastName)
      .map(_.name)

  def nameOfEarliestFilmByDirectorWithLastNameOrFailure(
    directors: List[Director],
    lastName: String
  ): Either[String, String] =
    nameOfEarliestFilmByDirectorWithLastName(directors, lastName).toRight(
      "No Film Found"
    )

  def main(args: Array[String]): Unit = {
    import films.TestData._

    println("directorWithLastName")
    // println(directorWithLastName(directors, "Nolan"))
    // println(directorWithLastName(directors, "Guy"))
    // println(directorWithLastName(directors, "DROP TABLE *;"))

    println("directorWithLastNameOrFailure")
    // println(directorWithLastNameOrFailure(directors, "Nolan"))
    // println(directorWithLastNameOrFailure(directors, "Guy"))
    // println(directorWithLastNameOrFailure(directors, "DROP TABLE *;"))

    println("yearOfBirthOfDirectorWithLastName")
    // println(yearOfBirthOfDirectorWithLastName(directors, "Nolan"))
    // println(yearOfBirthOfDirectorWithLastName(directors, "Guy"))
    // println(yearOfBirthOfDirectorWithLastName(directors, "DROP TABLE *;"))

    println("yearOfBirthOfDirectorWithLastNameOrFailure")
    // println(yearOfBirthOfDirectorWithLastNameOrFailure(directors, "Nolan"))
    // println(yearOfBirthOfDirectorWithLastNameOrFailure(directors, "Guy"))
    // println(yearOfBirthOfDirectorWithLastNameOrFailure(directors, "DROP TABLE *;"))

    println("filmsByDirectorWithLastName")
    // println(filmsByDirectorWithLastName(directors, "Nolan"))
    // println(filmsByDirectorWithLastName(directors, "Guy"))
    // println(filmsByDirectorWithLastName(directors, "DROP TABLE *;"))

    println("earliestFilmByDirectorWithLastName")
    // println(earliestFilmByDirectorWithLastName(directors, "Nolan"))
    // println(earliestFilmByDirectorWithLastName(directors, "Guy"))
    // println(earliestFilmByDirectorWithLastName(directors, "DROP TABLE *;"))

    println("earliestFilmByDirectorWithLastNameOrFailure")
    // println(earliestFilmByDirectorWithLastNameOrFailure(directors, "Nolan"))
    // println(earliestFilmByDirectorWithLastNameOrFailure(directors, "Guy"))
    // println(earliestFilmByDirectorWithLastNameOrFailure(directors, "DROP TABLE *;"))

    println("namesOfFilmsByDirectorWithLastName")
    // println(namesOfFilmsByDirectorWithLastName(directors, "Nolan"))
    // println(namesOfFilmsByDirectorWithLastName(directors, "Guy"))
    // println(namesOfFilmsByDirectorWithLastName(directors, "DROP TABLE *;"))

    println("nameOfEarliestFilmByDirectorWithLastName")
    // println(nameOfEarliestFilmByDirectorWithLastName(directors, "Nolan"))
    // println(nameOfEarliestFilmByDirectorWithLastName(directors, "Guy"))
    // println(nameOfEarliestFilmByDirectorWithLastName(directors, "DROP TABLE *;"))

    println("nameOfEarliestFilmByDirectorWithLastNameOrFailure")
    // println(nameOfEarliestFilmByDirectorWithLastNameOrFailure(directors, "Nolan"))
    // println(nameOfEarliestFilmByDirectorWithLastNameOrFailure(directors, "Guy"))
    // println(nameOfEarliestFilmByDirectorWithLastNameOrFailure(directors, "DROP TABLE *;"))
  }
}
