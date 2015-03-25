package datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /** EXERCISE 3.25 */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  /** EXERCISE 3.26 */
  def maximum[A](t: Tree[A])( max: (A, A) => A ): A = t match {
    case Leaf(v) => v
    case Branch(l, r) => max(maximum(l)(max), maximum(r)(max))
  }

  /** EXERCISE 3.27 */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  /** EXERCISE 3.28 */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /** EXERCISE 3.29 */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))

  }
  def foldSize[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_+_)
  def foldMaximum[A](t: Tree[A])( max: (A, A) => A ): A =
    fold(t)(v => v)(max)
  def foldDepth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + _.max(_))
  def foldMap[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
