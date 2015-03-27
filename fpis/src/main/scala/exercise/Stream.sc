import laziness.{Empty, Cons, Stream}

Stream(1,2,3).headOption
val x = Cons(() => {println("Call"); 10}, () => Stream.empty[Int])
x.headOption
x.headOption
val y = Stream.cons(() => {println("Call"); 10}, Stream.empty[Int])
y.headOption.getOrElse(20)
y.headOption.getOrElse(20)
Stream(1,2,3,4).toList
Stream(1,2,3,4).take(3)
Stream(1,2,3,4).take(3).toList
Stream(1,2,3,4).take(4).toList
Stream(1,2,3,4).take(5).toList
Stream(1,2,3,4).drop(3).toList
Stream(1,2,3,4).drop(4).toList
Stream(1,2,3,4).drop(5).toList
Stream(1,2,3,4).takeWhile(_ % 2 == 0).toList
Stream(1,2,3,4).foldRight(0)( (a, acc) => acc + a * 2)
Stream(1,2,3,4).forAll(_ % 2 == 0)
Stream(1,2,3,4).forAll(_ == 5)
Stream(1,2,3,4).takeWhileViaFoldRight(_ % 2 == 0).toList
Stream(1,2,3,4).headOptionViaFoldRight
Stream(2,3,4).headOptionViaFoldRight
val notNeed: Cons[Int] = Cons(() => {println("Call"); 10}, () => Stream.empty[Int])
notNeed.h()
notNeed.h()
val need: Stream[Int] = Stream.cons[Int]({println("Call"); 10}, Stream.empty[Int])
need.headOption
need.headOption
Stream(1,2,3,4).map(_.toString).toList
Stream(1,2,3,4).filter(_ % 2 == 0).toList
Stream(1,2,3,4).flatMap(a => Cons(() => a.toString, () => Stream.empty[String])).toList
