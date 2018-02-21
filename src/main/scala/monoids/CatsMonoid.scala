package monoids

import cats.{Monoid => CMonoid}
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

object CatsMonoid extends App {

  val a = CMonoid[Int].combine(10, 20)
  val b = a.combine(10)

  //  Combine don't exists for subtypes
  val p = Option(10).combine(None)

  println(s"Should be 20: $a")
  println(s"Should be 30: $b")
  println(s"Should be Some(10): $p")
}
