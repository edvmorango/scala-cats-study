package monads

import cats.Eval



object EvaluationMain extends App{


  val memoized = {
    println("Eager and memoized (will happen before the statements)")
    math.random()
  }

  lazy val lz = {
    println("Lazy and memoized" )
    math.random()
  }

  def eager = {
    println("Eager and not memoized")
    math.random()
  }


  println("\nEager Memoized \n")

  println(memoized)
  println(memoized)

  println("\nLazy \n")

  println(lz)
  println(lz)

  println("\nEager not memoized \n")
  println(eager)
  println(eager)




}


object EvalMain extends App {

  val now = Eval.now(math.random()) // eager + memoized

  val later = Eval.later(math.random()) // lazy + memoized

  val always = Eval.always(math.random()) // lazy + not memoized!


  println("Now:")
  println(now)
  println(now.value)

  println("\nLater:")
  println(later)
  println(later.value)
  println(later.value)


  println("\nAlways:")
  println(always)
  println(always.value)
  println(always.value)

  println("\nnowString:")

  val nowString = Eval.now{ println("Evaluated right now"); 10}
    .map(x => 10.toString)

  println(nowString)
  println(nowString.value)



  println("\nChaining Later + Always (always you reavaluate always) :\n")

  val chaining = Eval.later{ println("Later"); 10}.flatMap{ l =>
    Eval.always{
      println("Always")
      2 * l
    }
  }

  println(chaining)
  println(chaining.value)
  println(chaining.value)
  println(chaining.value)
  println(chaining.value)


  println("\nChaining Later + Always with Impurity:\n")

  val impureChaining = Eval.later(10).flatMap{ l =>
    Eval.always{
      val rnd = math.random()
      println(s"Always: $rnd")
      rnd * l
    }
  }

  println(impureChaining)
  println(impureChaining.value)
  println(impureChaining.value)

  println("\nChaining Later + Always with memoize (always will be evaluated once)  \n")
  val impureToPure = impureChaining.memoize
  println(impureToPure)
  println(impureToPure.value)
  println(impureToPure.value)
 // memoize brings purity?

  def foldRightEval[A, B](els: List[A], cur: Eval[B])(f: (A, Eval[B]) => Eval[B]) : Eval[B] = { els match {
    case h :: t =>
      Eval.defer(f(h, foldRightEval(t, cur)(f)))
    case Nil =>
      cur
    }
  }

  def foldRight[A, B](els: List[A], cur: B)(f: (A, B) => B): B =
    foldRightEval(els, Eval.now(cur)  ) { (a,b) =>
      b.map(f(a,_))
    }.value




}