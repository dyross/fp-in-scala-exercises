package chapter4

object ex_4_1 {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      this map f getOrElse None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      this map Some.apply getOrElse ob
    }

    def filter(f: A => Boolean): Option[A] = {
      this flatMap { a =>
        if (f(a)) Some(a)
        else None
      }
    }
  }
  case class Some[+A](value: A) extends Option[A]
  case object None extends Option[Nothing]
}

object ex_4_2 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
      mean(xs map (x => math.pow(x - m, 2)))
    }
}

object ex_4_3 {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }
}

object ex_4_4 {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @scala.annotation.tailrec
    def go(l: List[Option[A]], accum: List[A]): Option[List[A]] = {
      l match {
        case Nil => Some(accum.reverse)
        case Some(h) :: t => go(t, h :: accum)
        case None :: _ => None
      }
    }

    go(a, Nil)
  }
}

object ex_4_5 {
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val builder = scala.collection.mutable.ListBuffer.empty[B]
    @scala.annotation.tailrec
    def go(l: List[A]): Option[List[B]] = {
      l match {
        case Nil =>
          Some(builder.toList)
        case h :: t =>
          f(h) match {
            case Some(value) =>
              builder += value
              go(t)
            case None => None
          }
      }
    }

    go(a)
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
