package functors

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object TreeFunctorInstance {

  implicit val treeFunctor = new Functor[Tree] {

    def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Leaf(l)      => Leaf(f(l))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))

    }

  }

}

object TreeFunctorInterface {

  def fmap[F[_], A, B](tree: F[A])(f: (A => B))(implicit functor: Functor[F]) =
    functor.map(tree)(f)

}

object TreeFunctorSyntax {

  implicit class treeSyntax[F[_], A](tree: F[A])(implicit functor: Functor[F]) {
    def fmap[B](f: A => B) = functor.map(tree)(f)
  }

}

object TreeFunctor extends App {
  import TreeFunctorInstance._

  val branch2 = Branch(Leaf(10), Leaf(15))
  val branch1 = Branch(branch2, Leaf(5))
  val tree: Tree[Int] = Branch(branch1, Leaf(0))

  println(s"Tree: $tree")
  println(
    s"Tree * 2: ${TreeFunctorInterface.fmap[Tree, Int, Int](tree)(v => v * 2)} ")

  val syntax = {
    import TreeFunctorSyntax._
    s"Tree * 10: ${tree.fmap(v => v * 10)}"
  }

  println(syntax)
}
