class SandBox {

  def x: Unit = println("Print x")

  val y = ()

}

trait Meta
case class Foo(foo: String) extends Meta
case class Bar(bar:String) extends Meta

sealed trait Option[+A] {
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }
}
case class Some[+A](get: A) extends Option[A]

//val a: Some[Meta] = Some(Foo("Foo"))


//sealed trait OptionB[+A] {
//  def getOrElse[A](default: => A): A = this match {
//    case SomeB(v) => v
//    case NoneB => default
//  }
//}
//case class SomeB[+A](get: A) extends OptionB[A]
//case object NoneB extends OptionB[Nothing]
