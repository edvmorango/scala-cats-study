package foldtransv

object FoldMain extends App{
  import scala.math.Numeric

  val list: List[Int] = List(1, 2 , 3)

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


  def foldMapR[A,B](list: List[A])(f: A => B) = list.foldRight(List[B]())( (c, acc) =>  f(c) :: acc )
  def foldFlatMapR[A,B](list: List[A])(f: A => List[B] ) = list.foldRight(List[B]())((c, acc) =>  f(c) ::: acc  )
  def foldFilterR[A](list: List[A])(f: A => Boolean) = list.foldRight(List[A]())( (c, acc) => if(f(c))  (c :: acc) else acc )
  def foldSum[A](list: List[A])(implicit numeric: Numeric[A]) = list.foldRight(numeric.zero)(numeric.plus)

  println(foldMapR(list)(_.toString.concat("!")))
  println(foldFlatMapR(list)( x =>  List(x, x * 2)))
  println(foldFilterR(list)(x => x > 1))
  println(foldSum(list))

}
