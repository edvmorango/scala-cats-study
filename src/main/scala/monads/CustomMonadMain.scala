package monads

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object TreeInstance {

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {

    override def pure[A](value: A): Tree[A] = Leaf(value)

    override def flatMap[A, B](m: Tree[A])(func: (A) => Tree[B]): Tree[B] =
      m match {
        case Leaf(a)      => func(a)
        case Branch(a, b) => Branch(flatMap(a)(func), flatMap(b)(func))
      }

    override def map[A, B](m: Tree[A])(func: (A) => B): Tree[B] =
      m match {
        case Leaf(a)      => pure(func(a))
        case Branch(a, b) => Branch(map[A, B](a)(func), map(b)(func))
      }
  }
}

object TreeSyntax {

  implicit class MonadSyntax[F[_], A](m: F[A]) {

    def bind[B](f: (A => F[B]))(implicit mi: Monad[F]): F[B] =
      mi.flatMap(m)(f)

    def smap[B](f: (A => B))(implicit mi: Monad[F]): F[B] = mi.map(m)(f)

  }

}

object TreeInterface {

  def pure[F[_], A](value: A)(implicit m: Monad[F]): F[A] = {
    m.pure(value)
  }

  def flatMap[F[_], A, B](wrappedValue: F[A])(fb: A => F[B])(
      implicit m: Monad[F]): F[B] = {
    m.flatMap(wrappedValue)(fb)
  }

  def map[F[_], A, B](wrappedValue: F[A])(fb: A => B)(
      implicit m: Monad[F]): F[B] = {
    m.map(wrappedValue)(fb)
  }
}

object CustomMonadMain extends App {
  import TreeInstance._
  import TreeSyntax._

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A) = Leaf(value)

  val tree: Tree[Int] = Branch[Int](Branch(Leaf(1), Leaf(2)), Leaf(3))

  println(tree.smap(_ * 2))

}
