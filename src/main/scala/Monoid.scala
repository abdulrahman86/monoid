import Par._

trait Monoid[A]{

  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  implicit object StringMonoid extends Monoid[String] {

    override def op(a1: String, a2: String): String = a1.concat(a2)

    override def zero: String = ""
  }

  implicit object ListMonoid extends Monoid[List[_]] {
    override def op(a1: List[_], a2: List[_]): List[_] = a1 ::: a2

    override def zero: List[_] = List()
  }

  implicit val intAdditionMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplicationMonoid = new Monoid[Integer] {
    override def op(a1: Integer, a2: Integer): Integer = a1 * a2

    override def zero: Integer = 0
  }

  val booleanOrMonoid = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAndMonoid = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  implicit object OptionMonoid extends Monoid[Option[_]] {
    override def op(a1: Option[_], a2: Option[_]): Option[_] = a1 orElse a2

    override def zero: Option[_] = None
  }

  implicit def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {

    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1.andThen(a2)

    override def zero: (A) => A = x => x
  }

  implicit def parMonoid[A](implicit monoid: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {

      override def op(a1: Par[A], a2: Par[A]): Par[A] = {
        map2(monoid.op)(a1)(a2)
      }

      override def zero: Par[A] = unit(monoid.zero)
    }


  def parFoldMap[A, B](v: IndexedSeq[A])(f: A => B)(implicit m: Monoid[Par[B]]): Par[B] = {
    if (v.length == 0) {
      m.zero
    }
    else if (v.length == 1) {
      m.op(m.zero, unit(f(v.last)))
    }
    else {
      val (a, b) = v.splitAt(v.length / 2)
      m.op(fork(parFoldMap(a)(f)), fork(parFoldMap(b)(f)))
    }
  }

  def concatenate[A: Monoid](in: List[A])(implicit monoid: Monoid[A]): A =
    in.foldRight(monoid.zero)((a1, a2) => monoid.op(a1, a2))

  def foldMap[A, B](as: List[A])(f: A => B)(implicit monoid: Monoid[B]): B = concatenate(as.map(f))

  def foldMapV[A, B](v: IndexedSeq[A])(f: A => B)(implicit m: Monoid[B]): B = {

    if (v.length > 1) {

      val (a, b) = v.splitAt(v.length / 2);

      m.op(foldMapV(a)(f), foldMapV(b)(f))
    }

    v.lastOption.map(f) getOrElse(m.zero)

  }
}
