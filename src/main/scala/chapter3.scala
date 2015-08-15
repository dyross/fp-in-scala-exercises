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

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

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

object ex_3_7 {
  // foldRight can't early return as defined.
  // Because of association and argument evaluation,
  // it traverses entire list before looking at any element.
}

object ex_3_8 {
  // foldRight is pretty similar to Cons itself.
  List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
}

object ex_3_9 {
  def length[A](as: List[A]): Int =
    List.foldRight(as, 0)((_, n) => n + 1)
}

object ex_3_10 {
  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
}

object ex_3_11 {
  import ex_3_10._

  def sum(xs: List[Int]): Int =
    foldLeft(xs, 0)(_ + _)

  def product(xs: List[Int]): Int =
    foldLeft(xs, 1)(_ * _)

  def length[A](xs: List[A]): Int =
    foldLeft(xs, 0)((a, b) => a + 1)
}

object ex_3_12 {
  import ex_3_10._

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A]) {
      case (accum, x) => Cons(x, accum)
    }
}

object ex_3_13 {
  import ex_3_10._
  import ex_3_12._

  def foldLeft1[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    List.foldRight(as, z)((b, a) => f(a, b))

  def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))
}

object ex_3_14 {

  def append[A](l: List[A], r: List[A]): List[A] =
    List.foldRight(l, r)(Cons(_, _))
}

object ex_3_15 {
  import ex_3_10._
  import ex_3_14._

  def concatenateLists[A](l: List[List[A]]): List[A] = {
    List.foldRight(l, Nil: List[A])(append)
  }
}

object ex_3_16 {
  def addOne(l: List[Int]): List[Int] = {
    List.foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
  }
}

object ex_3_17 {
  def doubleToString(l: List[Double]): List[String] = {
    List.foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
  }
}

object ex_3_18 {
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = scala.collection.mutable.ListBuffer.empty[B]

    @scala.annotation.tailrec
    def go(l: List[A]): List[B] = l match {
      case Cons(h, t) =>
        buf += f(h)
        go(t)
      case Nil => List(buf: _*)
    }

    go(l)
  }
}

object ex_3_19 {
  def filter[A](l: List[A])(p: A => Boolean): List[A] = {
    val buf = scala.collection.mutable.ListBuffer.empty[A]

    @scala.annotation.tailrec
    def go(l: List[A]): List[A] = l match {
      case Cons(h, t) =>
        if (p(h))
          buf += h
        go(t)
      case Nil => List(buf: _*)
    }

    go(l)
  }

  filter(List(1, 2, 3, 4))(_ % 2 == 0)
}

object ex_3_20 {
  import ex_3_15._
  import ex_3_18._
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concatenateLists(map(l)(f))
  }
}

object ex_3_21 {
  import ex_3_20._

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l) { h =>
      if (f(h))
        List(h)
      else
        Nil
    }
  }
}

object ex_3_22 {

  def addLists(l1: List[Int], l2: List[Int]): List[Int] = {
    val buf = scala.collection.mutable.ListBuffer.empty[Int]

    @scala.annotation.tailrec
    def go(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        buf += h1 + h2
        go(t1, t2)
      case (Nil, Nil) => List(buf: _*)
      case _ => sys.error("Different sized lists")
    }

    go(l1, l1)

  }
}

object ex_3_23 {
  def zipWith[A, B, C](la: List[A], lb: List[B])(f: (A, B) => C): List[C] = {
    val buf = scala.collection.mutable.ListBuffer.empty[C]

    @scala.annotation.tailrec
    def go(la: List[A], lb: List[B]): List[C] = (la, lb) match {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        buf += f(h1, h2)
        go(t1, t2)
      case (Nil, Nil) => List(buf: _*)
      case _ => sys.error("Different sized lists")
    }

    go(la, lb)
  }
}

object ex_3_24 {

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {

    @scala.annotation.tailrec
    def startsWithSubsequence(l: List[A], sub: List[A]): Boolean = (l, sub) match {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 != h2) false
        else startsWithSubsequence(t1, t2)
      case (_, Nil) => true
      case (Nil, _) => false
    }

    @scala.annotation.tailrec
    def go(l: List[A]): Boolean = {
      if (startsWithSubsequence(l, sub)) true
      else l match {
        case Cons(_, t) => go(t)
        case Nil => false
      }
    }

    go(l)
  }
}

object ex_3_25 {

  def countNodes[A](tree: Tree[A]): Int = {
    @scala.annotation.tailrec
    def go(node: Tree[A], count: Int, remaining: List[Tree[A]]): Int = {
      node match {
        case Leaf(_) => remaining match {
          case Nil => count + 1
          case Cons(h, t) => go(h, count + 1, t)
        }

        case Branch(left, right) => go(left, count + 1, Cons(right, remaining))
      }
    }
    go(tree, 0, Nil)
  }
}

object ex_3_26 {

  def maximum(tree: Tree[Int]): Int = {
    @scala.annotation.tailrec
    def go(node: Tree[Int], curMax: Int, remaining: List[Tree[Int]]): Int = {
      node match {
        case Leaf(value) =>
          val newMax = curMax max value
          remaining match {
            case Nil => newMax
            case Cons(h, t) => go(h, newMax, t)
          }

        case Branch(left, right) => go(left, curMax, Cons(right, remaining))
      }
    }
    go(tree, 0, Nil)
  }
}

object ex_3_27 {
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }
}

object ex_3_28 {

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
}

object ex_3_29 {

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(a => 1)(1 + _ + _)
  }

  def maxViaFold(tree: Tree[Int]): Int = {
    fold(tree)(a => a)(_ max _)
  }

  def depthViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(a => 0)((x, y) => 1 + (x max y))
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }
}
