case class Matryoshka(
  number: Int,
  content: Seq[Any]
)


object Matryoshka {

  def main(): Unit = {
    val m = Matryoshka(
      1, Seq(Matryoshka(
        11, Seq(
          Matryoshka(111, Seq("Hoge")), Matryoshka(112, Seq(3L, 2L)))
      ))
    )

    val n = m.copy(number = 3)

    m match {
      case n => println(1, n)
      case m => println(2, m)
      case _ => println(3)
    }

    val M = m
    val N = n

    m match {
      case N => println(1, N)
      case M => println(2, M)
      case _ => println(3)
    }

    m match {
      case Matryoshka(_, Seq(m @ Matryoshka)) => println(1, m)
      case Matryoshka(_, Seq(m @ Matryoshka(_, _))) => println(2, m)
      case Matryoshka(_, Seq(m, _*)) => println(3, m)
      case _ => println(4)
    }

    m.content match {
      case c: Seq[Matryoshka] => println(1, c)
      case _ => println(2)
    }

    m match {
      case m: Matryoshka if false => println(1, m)
      case _ => println(2)
    }

    val l1 = List(Some(1), Some(2), Some(3))
    val l2 = List(Some(21), None, Some(23))
    val l3 = List(Some(31), Some(32), Some(33))

    for {
      Some(one) <- l1
      Some(two) <- l2
      Some(three) <- l3
    } yield println(one, two, three)
  }

}
