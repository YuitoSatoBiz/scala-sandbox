/**
  * @author yuito.sato
  */
object MyList {
  def apply[A](as: A*): MyList[A] = ???
}

sealed trait MyList[+A] {

  def isEmpty: Boolean

  def map[B](f: A => B): MyList[B]
  
  def ::[C >: A](x: C): MyList[C] = MyCons(x, this)

  def :::[C >: A](prefix: MyCons[C]): MyList[C]
}
case class MyCons[B](head: B, tail: MyList[B]) extends MyList[B] {
  override def isEmpty: Boolean = false

  override def map[R](f: (B) => R): MyList[R] = {
    if (isEmpty) {
      MyNil
    } else {
      MyCons(f(head), tail.map(f))
    }
  }

  override def :::[C >: B](prefix: MyCons[C]): MyList[C] = {
    if (prefix.isEmpty) {
      this
    } else {
      prefix.head :: prefix.tail
    }
  }

}

case object MyNil extends MyList[Nothing] {

  override def isEmpty: Boolean = true

  override def map[R](f: (Nothing) => R): MyList[R] = MyNil

  override def :::[C >: Nothing](prefix: MyCons[C]): MyList[C] = prefix
}
