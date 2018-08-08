

sealed trait MyEither[+L, +R] {

  def isRight: Boolean

  def isLeft: Boolean = !isRight

  def map[RR](f: R => RR): MyEither[L, RR]

  def flatMap[RR, LL >: L](f: R => MyEither[LL, RR]): MyEither[LL, RR]

  def foreach(f: R => Unit): Unit = map(f)

  def fold[C](fl: L => C, fr: R => C): C = this match {
    case MyLeft(l) => fl(l)
    case MyRight(r) => fr(r)
  }

}

case class MyRight[+L, +R](
  value: R
) extends MyEither[L, R] {

  override def isRight: Boolean = true

  override def map[RR](f: R => RR): MyEither[L, RR] = MyRight(f(value))

  override def flatMap[RR, LL >: L](f: R => MyEither[LL, RR]): MyEither[LL, RR] = f(value)

}

case class MyLeft[+L, +R](
  value: L
) extends MyEither[L, R] {

  override def isRight: Boolean = false

  override def map[RR](f: R => RR): MyEither[L, RR] = this.asInstanceOf[MyLeft[L, RR]]

  override def flatMap[RR, LL >: L](f: R => MyEither[LL, RR]): MyEither[LL, RR] = this.asInstanceOf[MyLeft[L, RR]]

}
