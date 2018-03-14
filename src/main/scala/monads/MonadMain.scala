package monads

object MonadInstance {

  implicit val optionMonad: Monad[Option] = new Monad[Option] {

    override def pure[A](value: A): Option[A] = Option(value)

    override def flatMap[A, B](m: Option[A])(
        func: (A) => Option[B]): Option[B] = m.flatMap(func)

    override def map[A, B](m: Option[A])(func: (A) => B): Option[B] =
      flatMap(m)(v => pure(func(v)))
  }

}

object MonadInterface {

  def pure[F[_], A](value: A)(implicit m: Monad[F]): F[A] = {
    m.pure(value)
  }

  def flatMap[F[_], A, B](wrappedValue: F[A])(fb: A => F[B])(
      implicit m: Monad[F]): F[B] = {
    m.flatMap(wrappedValue)(fb)
  }

}

object MonadSyntax {

  implicit class MonadSyntaxWrapper[A](v: A) {
    def wrap[F[_]]()(implicit m: Monad[F]): F[A] = m.pure(v)
  }

  implicit class MonadSyntax[F[_], A](m: F[A]) {

    def bind[B](f: (A => F[B]))(implicit mi: Monad[F]): F[B] = mi.flatMap(m)(f)

    def smap[B](f: (A => B))(implicit mi: Monad[F]): F[B] = mi.map(m)(f)

  }

}

object MonadProofs {

  def rightId[F[_], A](value: F[A])(implicit m: Monad[F]): Boolean =
    m.flatMap(value)(m.pure) == value

  def leftId[F[_], A, B](value: A)(func: A => F[B])(
      implicit m: Monad[F]): Boolean = {
    val w = m.pure(value)

    m.flatMap(w)(x => func(x)) == func(value)
  }

  def associativity[F[_], A, B, C](value: A)(func: A => F[B])(func2: B => F[C])(
      implicit m: Monad[F]): Boolean = {

    val ml = m.flatMap(m.pure(value))(func)
    val left = m.flatMap(ml)(func2)

    val mr = m.pure(value)
    val right = m.flatMap(mr)(x => m.flatMap(func(x))(func2))

    left == right
  }

}

object MonadMain extends App {
  import MonadInstance._

  val option = MonadInterface.pure(10)
  val intToDouble = (a: Int) => Option(a.toDouble)

  val result = MonadInterface.flatMap(option)(intToDouble)

  println(s"Option Monad: $result")

  val resultSyntax = {
    import MonadSyntax._

    val soption: Option[Int] = 10.wrap()

//    val sresult: Option[Double] = soption.smap[Double](_.toDouble)
//    val sresult: Option[Double] = 10.wrap().bind[Double](x => Option(x.toDouble)  )
  }

  println(s"Option Monad Syntax: $result")

  val right = MonadProofs.rightId(option)
  val left = MonadProofs.leftId(10)(Option(_))
  val assoc = MonadProofs.associativity(10)(Option(_))(x => Option(x * 2))

  println(s"Right: $right Left: $left Assoc: $assoc")

}
