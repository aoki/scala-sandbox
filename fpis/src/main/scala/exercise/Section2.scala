package exercise

import scala.annotation.tailrec

object Section2 {

  /** EXERCISE 2.1 */
  def fib(n: Int): Int = {
    require(n > 0)

    @tailrec
    def go(x: Int, y: Int, c: Int): Int = {
      if (c == n) {
        x + y
      } else {
        go(y, x + y, c + 1)
      }
    }

    n match {
      case 1 => 0
      case 2 => 1
      case _ => go(0, 1, 3)
    }
  }

  /** EXERCISE 2.2 */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def go(x: A, y: Array[A]): Boolean = {
      println(s"$x, ${y.head}")
      if (y.length == 1) {
        ordered(x, y.head)
      } else if (!ordered(x, y.head)) {
        println(s"$x, ${y.head} => ordered: ${ordered(x, y.head)}")
        false
      } else {
        go(y.head, y.tail)
      }
    }

    go(as.head, as.tail)
  }

  /** EXERCISE 2.3 */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => f(a, _)
  }

  /** EXERCISE 2.4 */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /** EXERCISE 2.5 */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}
