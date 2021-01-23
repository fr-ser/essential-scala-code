package part2

// ----------------------------------------------

// Step 1. Implement an ADT `JsValue` to represent unstructured JSON data

sealed abstract class JsValue

// Include subtypes for each of the main types of JSON

case class JsString(x: String) extends JsValue
case class JsNumber(x: Double) extends JsValue
case class JsArray(x: List[JsValue]) extends JsValue
case class JsObject(x: List[(String, JsValue)]) extends JsValue
case object JsNull extends JsValue {}

// ----------------------------------------------

// Step 2. Implement a `stringify` method on the companion object for `JsValue`

object JsValue {
  private def escapeString(s: String) =
    s.flatMap(c => if (c == '"') s"\\$c" else c.toString())

  def stringify(json: JsValue): String = {
    json match {
      case JsString(x) => s""""${escapeString(x)}""""
      case JsNumber(x) => s"""$x"""
      case JsArray(x)  => s"""[${x.map(stringify).mkString(",")}]"""
      case JsObject(x) =>
        s"""{${x
          .map(keyValue =>
            s""""${escapeString(keyValue._1)}":${stringify(keyValue._2)}"""
          )
          .mkString(",")}}"""
      case JsNull => "null"
    }
  }
}

// ----------------------------------------------

object Exercise15Json {
  // val json1: JsValue =
  //   JsString("hello")

  // val json2: JsValue =
  //   JsArray(List(
  //     JsNumber(1),
  //     JsNumber(2),
  //     JsNumber(3),
  //   ))

  // val json3: JsValue =
  //   JsNull

  // val json4: JsValue =
  //   JsObject(List(
  //     ("foo", json1),
  //     ("bar", json2),
  //     ("baz", json3),
  //   ))

  def main(args: Array[String]): Unit = {
    println("stringify")
    // println(JsValue.stringify(json1))
    // println(JsValue.stringify(json2))
    // println(JsValue.stringify(json3))
    // println(JsValue.stringify(json4))
  }
}
