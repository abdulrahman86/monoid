import java.util.concurrent.{ExecutorService, Executors}

import scala.Some

/**
  * Created by asattar on 2016-10-13.
  */

object TestMonoid {

  import Par._
  import scala.concurrent.ExecutionContext.global

  import Monoid._

  def main(args: Array[String]): Unit = {
    assert(concatenate(List("abc", "def")) == "abcdef")

    //---Parallel foldMap Test----
//    val pool: ExecutorService = Executors.newFixedThreadPool(30)
//    val resultPar: Par[Int] = parFoldMap(List.range(1, 15, 1).to[IndexedSeq])(x=>x)
//    print(resultPar(pool).get())


    //---Check order of indexed sequence
    implicit def monoid[A](base: A) = new Monoid[A => Option[A]] {

      override def op(a1: (A) => Option[A], a2: (A) => Option[A]): (A) => Option[A] =
        in => a1(in).flatMap(a2)

      override def zero: (A) => Option[A] = _ => Some(base)
    }

    def func(a: Int) : Int => Option[Int] = x => if(x <= a) Some(a) else None

    println(foldMap(List(1, 2, 3, 4, 5))(func)(monoid[Int](0))(0))
  }
}
