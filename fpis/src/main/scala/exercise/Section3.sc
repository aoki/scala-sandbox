import datastructures.{List => L}
import datastructures.Nil

L.x
L.tail(L[Int](1,2,3,4,5))
L.tail(L[Int](1))
L.tail(Nil)
L.setHead(100, L[Int](1,2,3,4,5))
L.setHead(200, L[Int](1))
L.setHead(300, Nil)
L.drop(L[Int](1,2,3,4,5), 3)
L.drop(L[Int](1), 3)
L.drop(Nil, 3)
L.dropWhile(L[Int](1,2,3,4,5), (x: Int) => x < 4)
L.dropWhile(L[Int](1), (x: Int) => x < 4)
L.dropWhile(Nil, (x: Int) => x < 4)
L.init(L[Int](1,2,3,4,5))
L.init(L[Int](1))
L.init(Nil)
L.dropWhile2(L[Int](1,2,3,4,5))(_ < 4)
L.dropWhile2(L[Int](1))(_< 4)
L.dropWhile2(L[Int]())(_ < 4)
L.sum(L[Int](1,2,3,4,5))
L.∑\(L[Int](1,2,3,4,5))
L.product(L[Double](1.0,2.0,3.0,4.0,5.0))
L.∏\(L[Double](1.0,2.0,3.0,4.0,5.0))
L.x_3_8
L.length(L[Int](1,2,3,4,5))
L.length(L[Int](1,2,3))
L.length(L[Int]())
L./∑(L[Int](1,2,3,4,5))
L./∏(L[Double](1.0,2.0,3.0,4.0,5.0))
L.rightLength(L[Int](1,2,3,4,5))
L.reverse(L[Int](1,2,3,4,5))
L.sumFLuFR(L[String]("1","2","3","4","5"))
L.sumFL(L[String]("1","2","3","4","5"))
val il = L[Int](1,2,3,4,5)
val sl = L[String]("1","2","3","4","5")
L.foldLeft(sl, "0")((a: String, b:String) => s"$a + $b")
L.foldRight(sl, "0")((a: String, b:String) => s"$a + $b")
L.appendFL(sl, 6)
L.appendFR(sl, 6)
L.append(sl, L[String]("6", "7"))
val il2 = L(L[Int](1,2), L[Int](3,4), L[Int](5,6), L[Int](7,8))
L.flatFL(il2)
L.flatFR(il2)
L.flat(il2)

L.addOne(L[Int](1,2,3))
L.toString(L[Double](1.2,3.4,5.6))
L.map(L[Int](1,2,3))(_.toString)
L.filter(L[Int](1,2,3,4,5,6))( _ % 2 == 0)
L.flatMap(L(1,2,3))(i => L(i, i))
L.filterViaFlatMap(L[Int](1,2,3,4,5,6))( _ % 2 == 0)
L.addList(L(1,2,3), L(4,5,6))(_+_)
L.startsWith(L(1,2,3,4,5,6), L())
L.hasSubsequence(L(1,2,3,4,5,6), L(1,2))
L.hasSubsequence(L(1,2,3,4,5,6), L(2,3))
L.hasSubsequence(L(1,2,3,4,5,6), L(5,6))
L.hasSubsequence(L(1,2,3,4,5,6), L(1,3))
L.hasSubsequence(L(1,1,3,4,5,6), L(1,3))
L.hasSubsequence(L(1,1,3,4,5,6), L(3,4,5))
