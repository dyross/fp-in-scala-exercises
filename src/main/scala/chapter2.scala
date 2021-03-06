package chapter2

object ex_2_1 {

  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int = {
      if (n <= 1) a
      else go(n - 1, b, a + b)
    }

    go(n, 0, 1)
  }

}

object ex_2_2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(n: Int): Boolean = {
      if ((n + 1) >= as.length) true
      else ordered(as(n), as(n + 1)) && loop(n + 1)
    }

    loop(0)
  }

}

object ex_2_3 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

}

object ex_2_4 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

}

object ex_2_5 {

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

}
