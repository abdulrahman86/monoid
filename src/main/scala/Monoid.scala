import Par._

trait Monoid[A]{

  def op: (A, A) => A
  def zero: A
}

object Monoid {

  implicit object StringMonoid extends Monoid[String] {

    override def op = (a1, a2) => a1.concat(a2)

    override def zero: String = ""
  }

  implicit object ListMonoid extends Monoid[List[_]] {
    override def op = (a1, a2) => a1 ::: a2

    override def zero: List[_] = List()
  }

  implicit val intAdditionMonoid = new Monoid[Int] {
    override def op = (a1 ,a2) => a1 + a2

    override def zero: Int = 0
  }

  val intMultiplicationMonoid = new Monoid[Integer] {
    override def op = (a1, a2) => a1 * a2

    override def zero: Integer = 0
  }

  val booleanOrMonoid = new Monoid[Boolean] {
    override def op = (a1, a2) => a1 || a2

    override def zero: Boolean = false
  }

  val booleanAndMonoid = new Monoid[Boolean] {
    override def op = (a1, a2) => a1 && a2

    override def zero: Boolean = true
  }

  implicit object OptionMonoid extends Monoid[Option[_]] {
    override def op = (a1, a2) => a1 orElse a2

    override def zero: Option[_] = None
  }

  implicit def productMonoid[A: Monoid, B: Monoid] = new Monoid[(A, B)] {

    override def op: ((A, B), (A, B)) => (A, B) =
      (a, b) => (implicitly[Monoid[A]].op(a._1, b._1), implicitly[Monoid[B]].op(a._2, b._2))

    override def zero: (A, B) = (implicitly[Monoid[A]].zero, implicitly[Monoid[B]].zero)
  }

  implicit def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {

    override def op= (a1, a2) => a1.andThen(a2)

    override def zero: (A) => A = x => x
  }

  implicit def parMonoid[A](implicit monoid: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {

      override def op = (a1, a2) => {
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
    in.foldLeft(monoid.zero)((a1, a2) => monoid.op(a1, a2))

  def foldMap[A, B](as: List[A])(f: A => B)(implicit monoid: Monoid[B]): B = concatenate(as.map(f))

  def foldMapV[A, B](v: IndexedSeq[A])(f: A => B)(implicit m: Monoid[B]): B = {

    if (v.length > 1) {

      val (a, b) = v.splitAt(v.length / 2);

      m.op(foldMapV(a)(f), foldMapV(b)(f))
    }

    v.lastOption.map(f) getOrElse(m.zero)

  }

  object WordCount {

    sealed trait WC
    case class Stub(chars: String) extends WC
    case class Par(lStub: String, words: Int, rStub:String) extends WC

    implicit object WCMonoid extends Monoid[WC] {

      override def op = {
        case (Par(lStub1, words1, rStub1), Par(lStub2, words2, rStub2)) => Par(lStub1, words1 + words2 + 1, rStub2)
        case (e: Stub, f: Par) => f.copy(lStub = e + f.lStub)
        case (e: Par, f: Stub) => e.copy(rStub = e.rStub + f)
        case (e: Stub, f: Stub) => e.copy(chars = e.chars + f.chars)

      }

      override def zero: WC = Stub("")
    }

    def wordCount(s: String): Int = {
      concatenate(stringToWC(" " +  s + " ")) match {
        case Par(_, w, _) => w
        case _ => 0
      }
    }

    private def stringToWC(s: String) : List[WC] = {

      if(s.length == 1) s match {
        case " " => return List(Par("", 0, ""))
        case a => return List(Stub(a))
      }

      val (a, b) = s.splitAt(s.length / 2)

      stringToWC(a) ++ stringToWC(b)

    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B) : B

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B) : B

    def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]) : B

    def concatenate[A](as: F[A])(m: Monoid[A]) : A =
      foldLeft(as)(m.zero)(m.op)

    def toList[A](as: F[A]): List[A] = foldLeft[A, List[A]](as)(List())((b, a) => b ::: List(a))
  }

  object Foldable {

    implicit object FoldableList extends Foldable[List] {

      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
        as.foldRight(z)(f)

      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)

      override def foldMap[A, B](as: List[A])(f: (A) => B)(m: Monoid[B]): B =
        as.map(f).foldLeft(m.zero)(m.op)

    }

    implicit object FoldableOption extends Foldable[Option] {

      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
        as.map(x => f(x, z)) getOrElse z

      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
        as.map(x => f(z, x)) getOrElse z

      override def foldMap[A, B](as: Option[A])(f: (A) => B)(m: Monoid[B]): B =
        as.map(f.andThen(m.op.curried(m.zero))) getOrElse m.zero
    }
  }
}
