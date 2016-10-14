import java.util.concurrent.{ExecutorService, Executors}

/**
  * Created by asattar on 2016-10-13.
  */

object TestMonoid {

  import Par._
  import scala.concurrent.ExecutionContext.global

  import Monoid._

  def main(args: Array[String]): Unit = {
    assert(concatenate(List("abc", "def")) == "abcdef")

    val pool: ExecutorService = Executors.newFixedThreadPool(30)

    val resultPar: Par[Int] = parFoldMap(List.range(1, 15, 1).to[IndexedSeq])(x=>x)
    print(resultPar(pool).get())
  }
}
