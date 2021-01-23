package part3

import films.{Director, Film}

object Exercise16aOptionAndEither {
  def directorWithLastName(
      directors: List[Director],
      lastName: String
  ): Option[Director] = directors.find(d => d.lastName == lastName)

  def directorWithLastNameOrFailure(
      directors: List[Director],
      lastName: String
  ): Either[String, Director] =
    directors.find(d => d.lastName == lastName) match {
      case None        => Left("Found nothing")
      case Some(value) => Right(value)
    }

  def yearOfBirthOfDirectorWithLastName(
      directors: List[Director],
      lastName: String
  ): Option[Int] = directors.find(_.lastName == lastName).map(_.yearOfBirth)

  def yearOfBirthOfDirectorWithLastNameOrFailure(
      directors: List[Director],
      lastName: String
  ): Either[String, Int] = directors.find(_.lastName == lastName) match {
    case None    => Left("No director found")
    case Some(d) => Right(d.yearOfBirth)
  }

  def filmsByDirectorWithLastName(
      directors: List[Director],
      lastName: String
  ): List[Film] =
    directors.find(_.lastName == lastName).map(_.films).getOrElse(Nil)

  def earliestFilmByDirectorWithLastName(
      directors: List[Director],
      lastName: String
  ): Option[Film] =
    directors
      .find(_.lastName == lastName)
      .filter(_.films != Nil)
      .map(
        _.films.reduce((acc, curr) =>
          if (acc.yearOfRelease < curr.yearOfRelease) acc
          else curr
        )
      )

  def earliestFilmByDirectorWithLastNameOrFailure(
      directors: List[Director],
      lastName: String
  ): Either[String, Film] = {
    directors
      .find(_.lastName == lastName) match {
      case None => Left("No Author found")
      case Some(value) =>
        value.films match {
          case Nil           => Left("Author has no films")
          case f: List[Film] => Right(f.sortBy(_.yearOfRelease).head)
        }
    }
  }

  def namesOfFilmsByDirectorWithLastName(
      directors: List[Director],
      lastName: String
  ): List[String] = {
    directors.filter(_.lastName == lastName).flatMap(_.films).map(_.name)
  }

  def nameOfEarliestFilmByDirectorWithLastName(
      directors: List[Director],
      lastName: String
  ): Option[String] = {
    directors
      .filter(_.lastName == lastName)
      .flatMap(_.films)
      .sortBy(_.yearOfRelease) match {
      case Nil           => None
      case l: List[Film] => Some(l.head.name)
    }
  }

  def nameOfEarliestFilmByDirectorWithLastNameOrFailure(
      directors: List[Director],
      lastName: String
  ): Either[String, String] = {
    directors
      .filter(_.lastName == lastName) match {
      case Nil => Left("No director with last name found")
      case l: List[Director] =>
        l.flatMap(_.films) match {
          case Nil => Left("No movies found for director")
          case films: List[Film] =>
            Right(films.sortBy(_.yearOfRelease).head.name)
        }
    }

  }

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
