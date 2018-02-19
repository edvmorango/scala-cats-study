package equality

import cats.syntax.eq._
import cats.syntax.option._
import cats.instances.int._
import cats.instances.option._
import cats.Eq

case class Product(id: Int, description: String)

trait ProductInstances {

  implicit val productEq: Eq[Product] = Eq.instance[Product] { (p, p2) =>
    p.id === p2.id
  }

}

object EqualityMain extends App with ProductInstances {

//  Eq ensures type safety
  val eq = Eq[Int]

  println(s"eqv: ${eq.eqv(10, 10)}")
  println(s"1 === 1: ${1 === 1}")
  println(1 =!= 2)

//  Eq is about the supertype value, so this examples don't work

//  Some(1) === Option.empty[Int]
//  Some(1) === Option(1)  // "Some" and "None" don't have syntax just "Option[+A]"
//  Some(1) === None

  println(s"Option(1) === Some(1) ${Option(1) === Some(1)}")
  println(s"Option.empty[Int] === None: ${Option.empty[Int] === None}")
  println(s"1.some === none[Int]: ${1.some === none[Int]}")

  val a = 1.some // a : Option(1)
  val b = Some(1) // b : Some(1)

  // Don't work
  //  b === a

  println(s"1.some === Some(1) ${a === b}")

  val iphone6 = Product(1, "iPhone6")
  val iphone7 = Product(2, "iPhone7")

  println(s"Products: ${iphone6 === iphone7}")

}
