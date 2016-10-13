/**
  * Created by asattar on 2016-10-13.
  */
object TestMonoid {

  import Monoid._

  def main(args: Array[String]): Unit = {
    assert(concatenate(List("abc", "def")) == "abcdef")
  }
}
