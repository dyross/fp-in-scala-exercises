package chapter3

package fpinscala.datastructures {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }
}

import fpinscala.datastructures._

object ex_3_1 {
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case _ => 101
  }
}

object ex_3_2 {
  def tail[A](as: List[A]): List[A] = as match {
    case Cons(x, xs) => xs
    case Nil => sys.error("Empty list")
  }
}

object ex_3_3 {
  def setHead[A](as: List[A], value: A): List[A] = as match {
    case Cons(_, xs) => Cons(value, xs)
    case Nil => sys.error("Empty list")
  }
}

object ex_3_4 {
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case otherwise => otherwise
  }
}

object ex_3_5 {
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case otherwise => otherwise
  }
}

object ex_3_6 {
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def init2[A](l: List[A]): List[A] = {
    val accum = scala.collection.mutable.ListBuffer.empty[A]

    @scala.annotation.tailrec
    def go(l: List[A]): List[A] = l match {
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => List(accum: _*)
      case Cons(x, xs) =>
        accum += x
        go(xs)
    }

    go(l)
  }
}
