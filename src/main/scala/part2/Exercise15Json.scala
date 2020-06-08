package part2

// ----------------------------------------------

// Step 1. Implement an ADT `JsValue` to represent unstructured JSON data

sealed abstract class JsValue
case class JsObject(kvp: List[(String, JsValue)]) extends JsValue
case class JsArray(elements: List[JsValue])       extends JsValue
case class JsString(value: String)                extends JsValue
case class JsNumber(value: Double)                extends JsValue
sealed trait JsBoolean                            extends JsValue
case object JsTrue                                extends JsBoolean
case object JsFalse                               extends JsBoolean
case object JsNull                                extends JsValue

// Include subtypes for each of the main types of JSON

// ----------------------------------------------

// Step 2. Implement a `stringify` method on the companion object for `JsValue`

object JsValue {
  def stringify(json: JsValue): String =
    json match {
      case JsObject(kvp)     => kvp.map { case (key, value) => s""""$key":${stringify(value)}""" }.mkString("{", ",", "}")
      case JsArray(elements) => elements.map(stringify).mkString("[", ",", "]")
      case JsString(value)   => s""""$value""""
      case JsNumber(value)   => value.toString()
      case JsTrue            => "true"
      case JsFalse           => "false"
      case JsNull            => "null"
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

  def main(args: Array[String]): Unit =
    println("stringify")
  // println(JsValue.stringify(json1))
  // println(JsValue.stringify(json2))
  // println(JsValue.stringify(json3))
  // println(JsValue.stringify(json4))
}
