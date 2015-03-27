package errorhandling

trait Validation[+E, +B] {
  def map[C](f: B => C): Validation[E, C] = this match {
    case Errors(es) => Errors(es)
    case Success(v) => Success(f(v))
  }

  def flatMap[EE >: E, C](f: B => Validation[EE, C]): Validation[EE, C] = this match {
    case Errors(es) => Errors(es)
    case Success(v) => f(v)
  }

//  def flatMap[E, C](f: B => Validation[E, C]): Validation[E, C] = this match {
//    case Errors(es) => Errors(es)
//    case Success(v) => f(v)
//  }


//  def orElse
}
case class Errors[+E](values: Seq[E]) extends Validation[E, Nothing]
case class Success[+B](values: B) extends Validation[Nothing, B]

object Validation {
  def Try[A](a: => A): Validation[Exception, A] =
    try Success(a)
    catch { case e: Exception => Errors(Seq(e)) }

//  def sequence()
//  def traverse()
}

