package foldtrav

object FoldMain extends App {
  import scala.math.Numeric

  val list: List[Int] = List(1, 2, 3)

  println(list.foldLeft(0)(_ - _))
  //  (0 - 1) = - 1
  //  (-1 - 2) =  -3
  // (-3 -3 )
  // -6

  println(list.foldRight(0)(_ - _))
  // 1 - ( -2 -  (3 - 0) )
  // 1 -  (2 - 3)
  // 1 - ( - 1 )
  // 2

  def foldMapR[A, B](list: List[A])(f: A => B) =
    list.foldRight(List[B]())((c, acc) => f(c) :: acc)
  def foldFlatMapR[A, B](list: List[A])(f: A => List[B]) =
    list.foldRight(List[B]())((c, acc) => f(c) ::: acc)
  def foldFilterR[A](list: List[A])(f: A => Boolean) =
    list.foldRight(List[A]())((c, acc) => if (f(c)) (c :: acc) else acc)
  def foldSum[A](list: List[A])(implicit numeric: Numeric[A]) =
    list.foldRight(numeric.zero)(numeric.plus)

  println(foldMapR(list)(_.toString.concat("!")))
  println(foldFlatMapR(list)(x => List(x, x * 2)))
  println(foldFilterR(list)(x => x > 1))
  println(foldSum(list))

}

object TraverseMain extends App {

  import cats.Applicative
  import cats.syntax.applicative._
  import cats.syntax.apply._
  import cats.instances.vector._
  import cats.instances.list._
  import cats.instances.option._
  import cats.data.Validated

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(
      f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (acc, c) =>
      (acc, f(c)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  println(s"Sequence: ${listSequence(List(Vector(1, 2), Vector(2, 3)))}")
  println(
    s"Sequence: ${listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))}")
  println

  // Failable

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  println(s"Process: ${process(List(2, 2, 3))}")
  println(s"Process: ${process(List(2, 4, 6))}")
  println

  // Validated

  type ErrorsAcc[A] = Validated[List[String], A]

  def processV(inputs: List[Int]): ErrorsAcc[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is invalid"))
      }
    }

  println(s"ProcessV: ${processV(List(2, 3, 5))}")
  println(s"ProcessV: ${processV(List(2, 6, 4))}")

}
