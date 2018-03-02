package monads

import cats.{Id, Monad => CMonad}

object IdInstance {

  implicit val monadId: Monad[Id] = {
    new Monad[Id] {

      override def pure[A](value: A): Id[A] = value : Id[A]

      override def flatMap[A, B](m: Id[A])(func: (A) => Id[B]): Id[B] = func(m)

      override def map[A, B](m: Id[A])(func: (A) => B): Id[B] = func(m)
    }
  }


}



object IdMain extends App{
  import IdInstance._

  val idInt: Id[Int] = 100


  val pureId = CMonad[Id].pure(100)
  val monadId = CMonad[Id].flatMap(100)(_ * 10)
  val mapId = CMonad[Id].map(10.0)(_.toInt)
  val unwrap: String =  CMonad[Id].pure("Id[String] to String")

  println(pureId)
  println(mapId)
  println(monadId)
  println(unwrap)



}
