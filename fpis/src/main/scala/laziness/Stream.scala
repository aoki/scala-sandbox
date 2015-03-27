package laziness

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
//    case Cons(h, t) => Option(h())
    case Cons(h, t) => Some(h())
  }

  /** EXERCISE 5.1 */
  def toList: List[A] = this match {
    case Empty => List.empty[A] // Nil?
    case Cons(h, t) => h() :: t().toList
  }

  /** EXERCISE 5.2 */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n != 0 => Cons(h, () => t().take(n-1) )
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n == 0 => Cons(h, () => t().drop(0) )
    case Cons(h, t) => t().drop(n-1)
    case _ => Empty
  }

  /** EXERCISE 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case Cons(h, t) => t().takeWhile(p)
    case _ => Empty
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /** EXERCISE 5.4 */
  def forAll(p: A => Boolean): Boolean = {
    foldRight(false)( (a, acc) => acc || p(a) )
  }

  /** EXERCISE 5.5 */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) => if (p(a)) Stream.cons(a, acc) else acc )

  /** EXERCISE 5.6 */
  def headOptionViaFoldRight: Option[A] =
    foldRight[Option[A]](None)( (a, acc) => Option(a) )
//  this match {
//    case Empty => None
//    case Cons(h, t) => Option(h())
//  }

  /** EXERCISE 5.7 */
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])( (a, acc) => Stream.cons( f(a) , acc) )

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])( (a, acc) => if (p(a)) Stream.cons(a , acc) else acc )

//  def append(s: Stream[A]): Stream[A] =
//    foldRight(s)( (a, acc) => Stream.cons(a, acc) )

//  def flatMap[B](f: A => Stream[B]): Stream[B] =
//    foldRight(Stream.empty[B])((a, acc) => f(a).map(aa => Stream.cons[B](aa, acc)))

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
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
