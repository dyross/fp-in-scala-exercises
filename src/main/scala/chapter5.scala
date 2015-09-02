package chapter5

package fpinscala.datastructures {
  sealed trait Stream[+A] {
    import Stream._

    /* Ex 5.1 */

    def toList: List[A] = {
      this match {
        case Cons(h, t) => h() :: t().toList
        case Empty => Nil
      }
    }

    def toList2: List[A] = {
      @scala.annotation.tailrec
      def go(s: Stream[A], accum: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: accum)
        case Empty => accum
      }

      go(this, Nil).reverse
    }

    /* Ex 5.2 */

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    @scala.annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    /* Ex 5.3 */

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => empty
    }

  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}

