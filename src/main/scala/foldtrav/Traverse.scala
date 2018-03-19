package foldtrav

import cats.Applicative

trait MyTraverse[F[_]] {

  def traverse[G[_]: Applicative, A, B](inputs: F[A])(func: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, B](inputs: F[G[B]]): G[F[B]] =
    traverse(inputs)(identity)

}
