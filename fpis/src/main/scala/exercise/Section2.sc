import exercise.Section2

//val f0 = Section2.fib(0)
val f1 = Section2.fib(1)
val f2 = Section2.fib(2)
val f3 = Section2.fib(3)
val x = Range(1, 10).map(Section2.fib)

Section2.isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x <= y)
Section2.isSorted(Array(1, 2, 3, 5, 4), (x: Int, y: Int) => x <= y)
Section2.isSorted(Array(1, 3, 2, 4, 5), (x: Int, y: Int) => x <= y)
def p1[A, B, C](a: A, f: (A, B) => C): B => C = {
  (b: B) => f(a, b)
}
val f = (x: Int, y: Int) => s"$x + $y = ${x + y}"
val ff2 = Section2.curry[Int, Int, String](f)
val ff3 = ff2(3)
ff3(10)
val g = (x: Int) => (y: Int) => s"$x - $y = ${x - y}"
val g2 = Section2.uncurry(g)
g2(3, 5)
val gg = Section2.compose((x: Int) => x.toString, (y: Double) => y.toInt )
gg(14.3)
Section2.x

Section2.List.tail(Section2.List[Int](1,2,3,4,5))
Section2.List.tail(Section2.List[Int](1))
Section2.List.tail(Section2.Nil)
Section2.List.setHead(100, Section2.List[Int](1,2,3,4,5))
Section2.List.setHead(200, Section2.List[Int](1))
Section2.List.setHead(300, Section2.Nil)
Section2.List.drop(Section2.List[Int](1,2,3,4,5), 3)
Section2.List.drop(Section2.List[Int](1), 3)
Section2.List.drop(Section2.Nil, 3)
Section2.List.dropWhile(Section2.List[Int](1,2,3,4,5), (x: Int) => x < 4)
Section2.List.dropWhile(Section2.List[Int](1), (x: Int) => x < 4)
Section2.List.dropWhile(Section2.Nil, (x: Int) => x < 4)
Section2.List.init(Section2.List[Int](1,2,3,4,5))
Section2.List.init(Section2.List[Int](1))
Section2.List.init(Section2.Nil)
Section2.List.dropWhile2(Section2.List[Int](1,2,3,4,5))(_ < 4)
Section2.List.dropWhile2(Section2.List[Int](1))(_< 4)
Section2.List.dropWhile2(Section2.List[Int]())(_ < 4)
