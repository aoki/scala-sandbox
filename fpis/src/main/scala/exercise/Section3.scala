package exercise

import scala.annotation.tailrec

object Section3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons[A](as.head, apply(as.tail: _*))
    }

    def ∑\(ints: List[Int]): Int = foldRight(ints, 0)( _ + _ )
    def ∏\(ds: List[Double]): Double = foldRight(ds, 1.0)( _ * _ )

    def sum(ints: List[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil         => 1.0
      case Cons(x, xs) => x * product(xs)
    }

    /** EXERCISE 3.2 */
    def tail[A](l: List[A]): List[A] = l match {
      case Cons(_, Nil) => Nil
      case Cons(_, xs)  => xs
      case _            => Nil
    }

    /** EXERCISE 3.3 */
    //    def setHead[A](h: A, l: List[A]): List[A] = Cons(h, List.tail(l))
    def setHead[A](h: A, l: List[A]): List[A] = l match {
      case Cons(_, xs)  => Cons(h, xs)
      case Cons(_, Nil) => Cons(h, Nil)
      case _            => Cons(h, Nil)
    }

    /** EXERCISE 3.4 */
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] =
      l match {
        case Cons(_, Nil)          => Nil
        case Cons(_, xs) if n == 1 => xs
        case Cons(_, xs)           => drop(xs, n - 1)
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
        case Cons(_, Nil)          => Nil
        case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
        case Cons(x, xs)           => Cons(x, init(xs))
      }

    /**
     * EXERCISE 3.7
     * ショート条件を追加するような関数を受け取れば出来そう
     */
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    /** EXERCISE 3.9 */
    def length[A](as: List[A]): Int = foldRight(as, 0)( (_: A, y: Int) => 1 + y)

    /** EXERCISE 3.10 */
    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    /** EXERCISE 3.11 */
    def /∑(ints: List[Int]): Int = foldLeft(ints, 0)( _ + _ )
    def /∏(ds: List[Double]): Double = foldLeft(ds, 1.0)( _ * _ )
    def rightLength[A](as: List[A]): Int = foldLeft(as, 0)( (y: Int, x: A) => 1 + y )

    /** EXERCISE 3.12 */
    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((y: List[A], x: A) => Cons(x, y) )

    /** EXERCISE 3.13 */
    def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      type BtoB = B => B
      def innerIdent:BtoB = (b:B) => b
      def combinerDelayer:(A, BtoB) => BtoB =
        (a: A, delayFunc: BtoB) => (b:B) => delayFunc(f(b, a))
      def go:BtoB = foldRight(as, innerIdent)(combinerDelayer)
      go(z)
//
//    def aaaa: ((A, BtoB) => BtoB) => BtoB = foldRight(as, innerIdent)
//    1,2,3,4,5  0   _+_
//    FR((1,2,3,4,5), (b:B) => delayFunc(f(b, a))
//       f(1, FR((2,3,4,5), (b:B) => delayFunc(f(b, a)))(f)
//         f(2, FR((3,4,5), (b:B) => delayFunc(f(b, a)))(f)
//           f(3, FR((4,5), (b:B) => delayFunc(f(b, a)))(f)
//             f(4, FR((5), (b:B) => delayFunc(f(b, a)))(f)
//               f(5, (b: B)=>B)
//    f(1, f(2, f(3, f(4, f(5, (b: B)=>B))))
//    (1 + (2 + (3 + (4 + (5 + (b: B)=>B))))
//    (b: String) => (1 + (2 + (3 + (4 + (5 + b))))
//
//
//
//
//    (comD)
    }

    def sumFL(ints: List[String]): String = foldLeft(ints, "0")( (x: String, y: String) => s"$x + $y")
    def sumFLuFR(ints: List[String]): String = foldLeftUsingFoldRight(ints, "0")( (x: String, y: String) => s"$x + $y")

    def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(as, z)( (b: B, a: A) => f(a, b) )

    /** EXERCISE 3.14 */
    def appendFL[A](as: List[A], a: A): List[A] = {
      foldLeft(
        foldLeft(as, Nil: List[A])((b: List[A], a: A) => Cons(a, b)), Cons(a, Nil: List[A])
      )( (b: List[A], a: A) => Cons(a, b))
    }
    def appendFR[A](as: List[A], a: A): List[A] = {
      foldRight(as, Cons(a, Nil))((a: A, b: List[A]) => Cons(a, b))
    }

    // 回答のappendはリストの結合だった。。
    def append[A](l: List[A], m: List[A]): List[A] = {
      foldRight(l, m)(Cons(_, _))
    }

    /** EXERCISE 3.15 */
    def flatFL[A](l: List[List[A]]): List[A] = {
      foldLeft(l, Nil: List[A])( (a: List[A], b: List[A]) => foldLeft(List.reverse(a), b)( (c: List[A], d: A) => Cons(d, c)) )
    }
    def flatFR[A](l: List[List[A]]): List[A] = {
      foldRight(l, Nil: List[A])( (a: List[A], b: List[A]) => foldRight(a, b)(Cons(_, _)) )
    }

    // 3.14の回答がリストの結合だった
    def flat[A](l: List[List[A]]): List[A] = {
      foldRight(l, Nil: List[A])(append)
    }

    /** EXERCISE 3.16 */
    def addOne(l: List[Int]): List[Int] = {
      List.reverse(List.foldLeft(l, Nil: List[Int])((y, x) => Cons(x + 1, y)))
    }

    /** EXERCISE 3.17 */
    def toString(ds: List[Double]): List[String] = {
      foldRight(ds, Nil: List[String])((d, l) => Cons(d.toString, l))
    }

    /** EXERCISE 3.18 */
    def map[A, B](as: List[A])(f: A => B): List[B] = {
      foldRight[A, List[B]](as, Nil: List[B])((a, l) => Cons(f(a), l))
    }

    /** EXERCISE 3.19 */
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
    }

    /** EXERCISE 3.20 */
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
      foldRight(as, Nil: List[B])((h, t) => List.append(f(h), t))
    }

    /** EXERCISE 3.21 */
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(a => if (f(a)) List(a) else Nil)
    }

    /** EXERCISE 3.22 */
    def addList[A](as: List[A], bs: List[A])(add: (A , A) => A): List[A] = {
      foldRight(as, Nil: List[A])((h, t) => flatMap(bs)(b => List(add(h, b))))
    }

    /** EXERCISE 3.23 */
    // 勘違いして、すでに出来てた
    def zipWith[A](as: List[A], bs: List[A])(add: (A , A) => A): List[A] = {
      foldRight(as, Nil: List[A])((h, t) => flatMap(bs)(b => List(add(h, b))))
    }

    /** EXERCISE 3.24 */
    @tailrec
    def startsWith[A](as: List[A], bs: List[A]): Boolean = (as, bs) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
      case _ => false
    }

    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, xs) => hasSubsequence(xs, sub)

    }
  }

  /** EXERCISE 3.1 */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <- Answer
    case Cons(h, t)                            => h + List.sum(t)
    case _                                     => 101
  }

  /** EXERCISE 3.8
    * 入力のコピーが生成される。
    * コンストラクタとの関係はコピー？
    */
  val x_3_8 = List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))

}
