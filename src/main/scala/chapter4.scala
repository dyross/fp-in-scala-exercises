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

object ex_4_6 {
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        aa <- this
        bb <- b
      } yield f(aa, bb)
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
}

import ex_4_6._

object ex_4_7 {

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    @scala.annotation.tailrec
    def go(as: List[A], accum: List[B]): Either[E, List[B]] = as match {
      case Nil => Right(accum.reverse)
      case a :: as => f(a) match {
        case Right(b) => go(as, b :: accum)
        case Left(e) => Left(e)
      }
    }
    go(as, Nil)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

}

object ex_4_8 {
  sealed trait NonEmptyList[+A]
  case class SingleItem[+A](a: A) extends NonEmptyList[A]
  case class Cons[+A](a: A, as: NonEmptyList[A]) extends NonEmptyList[A]

  type EitherNel[E, A] = Either[NonEmptyList[E], A]
}
