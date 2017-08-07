import java.nio.Buffer

/**
  * @author yuito.sato
  */
object MyList {

  def apply[A](head: A, tail: MyList[A]): MyList[A] = MyCons(head, tail)

  def test(): Unit = {
    val list1 = MyList(1, MyList(2, MyNil))
    val list2 = MyList(4, MyNil)
    val list3 = 3::list2
    val list4 = (list1 ::: list3).map { e =>
      e + 1
    }
    list4.foreach { e =>
      println(e)
    }
  }
}

sealed trait MyList[+A] {

  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def map[B](f: A => B): MyList[B]

  def foreach(f: A => Unit): Unit
  
  def ::[C >: A](x: C): MyList[C]

  def :::[C >: A](prefix: MyList[C]): MyList[C]
}

case class MyCons[A](hd: A, tl: MyList[A]) extends MyList[A] {

  override def head: A = hd

  override def tail: MyList[A] = tl

  override def isEmpty: Boolean = false

  override def map[B](f: (A) => B): MyList[B] = {
    if (isEmpty) {
      MyNil
    } else {
      MyCons(f(head), tail.map(f))
    }
  }

  override def foreach(f: (A) => Unit): Unit = {
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
  }

  override def ::[C >: A](x: C): MyList[C] = {
    MyCons(x, this)
  }

  override def :::[C >: A](prefix: MyList[C]): MyList[C] = {
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

  override def map[B](f: (Nothing) => B): MyList[B] = MyNil

  override def foreach(f: (Nothing) => Unit): Unit = ()

  override def ::[C >: Nothing](x: C): MyList[C] = MyCons(x, MyNil)

  override def :::[C >: Nothing](prefix: MyList[C]): MyList[C] = prefix
}
