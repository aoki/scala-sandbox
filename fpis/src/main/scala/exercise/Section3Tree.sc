import datastructures.{Branch => B}
import datastructures.{Leaf => L}
import datastructures.{Tree => T}

val t = B(
  B(L("a"), L("b")),
  B(L("c"), L("d"))
)
T.size(t)

val t2 = B(
  B(L(1), L(2)),
  B(L(3), B(
    L(2), L(7))
  )
)
T.maximum(t2)(_.max(_))

val t3 = B(
  B(L(1), L(2)),
  B(L(1), B(
    L(1), B(
      L(1), L(2))))
)
T.depth(t3)

val t4 = B(
  B(L("1"), L("2")),
  B(L("3"), L("4"))
)
T.map(t4)(_.toInt)

List(1,2)
