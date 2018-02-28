package functors

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

import cats.instances.future._

object FunctorInstance {

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa match {
      case Nil => List[B]()
      case e   => e.map(f)
    }
  }
}

object FunctorInterface {

  def fmap[A, B, F[A]](fa: F[A])(func: (A => B))(
      implicit functor: Functor[F]): F[B] =
    functor.map(fa)(func)

}

object FunctorSyntax {

  implicit class functorSyntax[F[A], A, B](fa: F[A])(
      implicit functor: Functor[F]) {
    def fmap(fm: (A => B)) = functor.map(fa)(fm)

  }

}

object MainFunctor extends App {
  import FunctorInstance._

  val list = List(1, 2, 3)

  println(s"$list  maps (_ * 2) => ${FunctorInterface.fmap(list)(_ * 2)}")

  val syntax = {
    import FunctorSyntax._

    val slist = List(1, 2, 3)

    s"$slist  maps (_ ^ 2) => ${slist.fmap(_ * 3)}"
  }

  println(syntax)

}
