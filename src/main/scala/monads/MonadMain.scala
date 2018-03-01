package monads


object MonadInstance {

  implicit val optionMonad: Monad[Option] = new Monad[Option] {

    override def pure[A](value: A): Option[A] = Option(value)

    override def flatMap[A, B](m: Option[A])(func: (A) => Option[B]): Option[B] = m.flatMap(func)

    override def map[A, B](m: Option[A])(func: (A) => B): Option[B] = m.map(func)
  }

}


object MonadInterface  {

  def pure[F[_], A](value: A)(implicit m: Monad[F]): F[A] = {
    m.pure(value)
  }

  def flatMap[F[_], A, B](wrappedValue: F[A])(fb: A => F[B])(implicit m: Monad[F]): F[B] = {
    m.flatMap(wrappedValue)(fb)
  }


}


object MonadProofs {
//
//  def leftId[F[_], A, B](value: A)(func: A => F[B])(implicit m: Monad[F[A]]): Boolean = {
//    val wrapped = m.pure(value)
//    m.flatMap(wrapped)(func) == func(value)
//  }
//
//  def rightId[F[_], A, B](value: A)(implicit m: Monad[F[A]]): Boolean = {
//    m.pure(value) == m
//  }

//  def associativity[F[_], A, B, C](value: A)(func: A => F[B])(func2: F[B => F[C])(implicit m: Monad[F[A]]): Boolean = {
//    val lwrapped = m.flatMap(m.pure(value))(func)
//    val left = m.flatMap(lwrapped)(func2)
//
//    val rwrapped = m.pure(func(value))
//    val right = m.flatMap(rwrapped)(func2)
//
//    left == right

//}


}




object MonadMain extends App{
  import MonadInstance._


  val option = MonadInterface.pure(10)
  val intToDouble = (a: Int) => Option(a.toDouble)

  val result = MonadInterface.flatMap(option)(intToDouble)

  println(s"Option Monad: $result")


}
