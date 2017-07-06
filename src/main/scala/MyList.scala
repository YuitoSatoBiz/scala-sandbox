/**
  * @author yuito.sato
  */
object MyList {
  def apply[A](as: A*): MyList[A] = ???
}

sealed trait MyList[+A] {

  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def map[B](f: A => B): MyList[B]
  
  def ::[C >: A](x: C): MyList[C]

  def :::[C >: A](prefix: MyList[C]): MyList[C]
}

case class MyCons[B](hd: B, tl: MyList[B]) extends MyList[B] {

  override def head: B = hd

  override def tail: MyList[B] = tl

  override def isEmpty: Boolean = false

  override def map[R](f: (B) => R): MyList[R] = {
    if (isEmpty) {
      MyNil
    } else {
      MyCons(f(head), tail.map(f))
    }
  }

  override def ::[C >: B](x: C): MyList[C] = {
    MyCons(x, this)
  }

  override def :::[C >: B](prefix: MyList[C]): MyList[C] = {
    if (prefix.isEmpty) {
      this
    } else {
      prefix.head :: prefix.tail ::: this
    }
  }
}

case object MyNil extends MyList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException("head of empty list")

  override def tail: MyList[Nothing] = throw new NoSuchElementException("tail of empty list")

  override def isEmpty: Boolean = true

  override def map[R](f: (Nothing) => R): MyList[R] = MyNil

  override def ::[C >: Nothing](x: C): MyList[C] = MyCons(x, MyNil)

  override def :::[C >: Nothing](prefix: MyList[C]): MyList[C] = prefix
}
