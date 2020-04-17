import $ivy.`org.typelevel::cats-core:2.2.0-M1`
import cats.implicits._

val x : Int = 5 
val s : String = "Hello World"

type MyProduct = (Int, String) // <-- this is called product type 

type MyProduct2 = (Boolean, Boolean)
                //    2       2     =   2 * 2  = 4
val distinctValues: List[MyProduct2] = List(
  (true, false),
  (false, true),
  (false, false),
  (true, true)
 )
// type Name = String 
// type Age  = Int 
// type Profession = String 
//type Person = (Name, Age, Profession)   // A person  *has a* name *and* an age *and* a profession

type Name = String 

type Person = (String, Int, String)   // A person  *has a* name *and* an age *and* a profession

val jan :  Person  = ("Hans", 25, "Software engineer")

println(jan._1)
println(jan._2)
println(jan._3)

case class Person2(name:String, age:Int, profession:String ) 

// A person *has a* name *and* an age *and* a profession

val leif : Person2 = Person2("Franz", 18, "Software engineer")

println(leif.profession)
case class Point(x : Int, y : Int )

// TrafficLight -> Red | Yellow | Green  // enum-ishprev
sealed trait TrafficLight   // Sum type 
object Red extends TrafficLight
object Yellow extends TrafficLight
object Green extends TrafficLight


// *Is-a* relationship
// TrafficLight *is* Red *or* Yellow *or* Green 

sealed trait Button
object Pushed extends Button
object NotPushed extends Button 
// A button *is* Pushed *or* not Pushed 

val pedestrian : TrafficLight = Red 

val result = pedestrian match {
    case Green => "green"
    case Red => "red"
    /// case Yellow => "yellow" // Missing case will result in compile error
    //case Defective => // error handling??
}
println(result)

// another file/class
object HalfPushed extends Button // <- give an error 


// Algebraic data types
// Sum types aka Coproduct -> sealed traits -> is a relationship
// Product typs -> tuple / case classes -> has a relationship

// |TrafficLight => Button| = |TrafficLight| ^ |Button|
// 3 => 2 3 ^ 2 = 9

// "red" "green"