import exercise.Section3

Section3.x

Section3.List.tail(Section3.List[Int](1,2,3,4,5))
Section3.List.tail(Section3.List[Int](1))
Section3.List.tail(Section3.Nil)
Section3.List.setHead(100, Section3.List[Int](1,2,3,4,5))
Section3.List.setHead(200, Section3.List[Int](1))
Section3.List.setHead(300, Section3.Nil)
Section3.List.drop(Section3.List[Int](1,2,3,4,5), 3)
Section3.List.drop(Section3.List[Int](1), 3)
Section3.List.drop(Section3.Nil, 3)
Section3.List.dropWhile(Section3.List[Int](1,2,3,4,5), (x: Int) => x < 4)
Section3.List.dropWhile(Section3.List[Int](1), (x: Int) => x < 4)
Section3.List.dropWhile(Section3.Nil, (x: Int) => x < 4)
Section3.List.init(Section3.List[Int](1,2,3,4,5))
Section3.List.init(Section3.List[Int](1))
Section3.List.init(Section3.Nil)
Section3.List.dropWhile2(Section3.List[Int](1,2,3,4,5))(_ < 4)
Section3.List.dropWhile2(Section3.List[Int](1))(_< 4)
Section3.List.dropWhile2(Section3.List[Int]())(_ < 4)

Section3.List.sum(Section3.List[Int](1,2,3,4,5))
Section3.List.∑(Section3.List[Int](1,2,3,4,5))
Section3.List.product(Section3.List[Double](1.0,2.0,3.0,4.0,5.0))
Section3.List.∏(Section3.List[Double](1.0,2.0,3.0,4.0,5.0))
