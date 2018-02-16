package show

import cats.Show
import cats.implicits._

case class Person(id: Int, name: String, age: Int)

object MainShow extends App {

//  implicit val personShowVerbose: Show[Person] = new Show[Person] {
//    override def show(person: Person): String =
//      s"Id: ${person.id} Name: ${person.name} Age: ${person.age}"
//  }

  implicit val personShow: Show[Person] = Show.show(person =>
    s"Id: ${person.id} \nName: ${person.name} \nAge: ${person.age}")

  val person = new Person(1, "Eduardo", 21)
  println(person.show)

}
