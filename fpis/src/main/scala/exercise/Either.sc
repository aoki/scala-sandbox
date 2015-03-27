import errorhandling.{Right, Left, Either}

Right(3).map(_*2)
//Left(3).map(_*2)
scala.util.Left(12).left.map(_ + 2) // Left(14)
scala.util.Right[Int, Int](12).left.map(_ + 2) // Right(12)
