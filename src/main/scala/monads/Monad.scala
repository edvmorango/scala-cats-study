package monads

trait Monad[F[_]] {
  def pure[A](value: A): F[A]
  def flatMap[A,B](m: F[A])(func: A => F[B]): F[B]
  def map[A,B](m: F[A])(func: A => B): F[B]
}
