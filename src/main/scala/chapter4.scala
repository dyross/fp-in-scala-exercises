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
