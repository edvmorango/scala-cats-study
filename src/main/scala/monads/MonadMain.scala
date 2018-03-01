package monads


object MonadProofs {

  def leftId[F[_], A, B](value: A)(func: A => B)(implicit m: Monad[F[A]]): Boolean = {
    val wrapped = m.pure(value)
    m.flatMap(wrapped)(func) == func(value)
  }

  def rightId[F[_], A, B](value: A)(implicit m: Monad[F[A]]): Boolean = {
    m.pure(value) == m
  }

  def associativity[F[_], A, B, C](value: A)(func: A => B)(func2: B => C)(implicit m: Monad[F[A]]): Boolean = {
    val lwrapped = m.flatMap(m.pure(value))(func)
    val left = m.flatMap(lwrapped)(func2)

    val rwrapped = m.pure(func(value))
    val right = m.flatMap(rwrapped)(func2)

    left == right

  }


}




object MonadMain extends App{

}
