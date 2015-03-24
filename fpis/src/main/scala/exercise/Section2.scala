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


  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons[A](as.head, apply(as.tail: _*))
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    /** EXERCISE 3.2 */
    def tail[A](l: List[A]): List[A] = l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs)  => xs
      case _            => Nil
    }

    /** EXERCISE 3.3 */
    //    def setHead[A](h: A, l: List[A]): List[A] = Cons(h, List.tail(l))
    def setHead[A](h: A, l: List[A]): List[A] = l match {
      case Cons(x, xs)  => Cons(h, xs)
      case Cons(x, Nil) => Cons(h, Nil)
      case _            => Cons(h, Nil)
    }

    /** EXERCISE 3.4 */
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] =
      l match {
        case Cons(x, Nil)          => Nil
        case Cons(x, xs) if n == 1 => xs
        case Cons(x, xs)           => drop(xs, n - 1)
        case _                     => Nil
      }

    /** EXERCISE 3.5 */
    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Cons(x, Nil) if f(x) => Nil
        case Cons(x, xs) if f(x)  => dropWhile(xs, f)
        case l                    => l
      }

    @tailrec
    def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
      l match {
        case Cons(x, Nil) if f(x) => Nil
        case Cons(x, xs) if f(x)  => dropWhile2(xs)(f)
        case l                    => l
      }

    /** EXERCISE 3.6 */
    def init[A](l: List[A]): List[A] =
      l match {
        case Nil                   => Nil
        case Cons(x, Nil)          => Nil
        case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
        case Cons(x, xs)           => Cons(x, init(xs))
      }
  }

  /** EXERCISE 3.1 */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + List.sum(t)
    case _                                     => 101
  }


}
