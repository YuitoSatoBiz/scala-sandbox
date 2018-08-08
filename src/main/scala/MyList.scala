import scala.annotation.tailrec

/**
  * @author yuito.sato
  */
object MyList {

  def apply[A](head: A, tail: MyList[A]): MyList[A] = MyCons(head, tail)

  def apply[A](seq: A*): MyList[A] = seq.foldRight(MyNil: MyList[A])((head, next) => head :: next)

  def test(): Unit = {}

}

sealed trait MyList[+A] {

  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean = this match {
    case MyNil => true
    case MyCons(_, _) => false
  }

  def foldLeft[B](zero: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(list: MyList[A], acc: B): B = list match {
      case MyNil => acc
      case MyCons(h, t) => loop(t, f(acc, h))
    }
    loop(this, zero)
  }

  def foldRight[B](zero: B)(f: (A, B) => B): B = {
    @tailrec
    def loop(list: MyList[A], acc: B => B): B => B = list match {
      case MyNil => acc
      case MyCons(h, t) => loop(t, b => acc(f(h, b)))
    }
    loop(this, b => b)(zero)
  }

  def ::[C >: A](x: C): MyList[C] = this match {
    case MyNil => MyCons(x, MyNil)
    case MyCons(_, _) => MyCons(x, this)
  }

  def :::[C >: A](prefix: MyList[C]): MyList[C] = foldRight(this)((head, list) => head :: list)

  def flatMap[B](f: A => MyList[B]): MyList[B] = foldRight(MyNil: MyList[B])((head, next) => f(head).head :: next)

  def map[B](f: A => B): MyList[B] = flatMap(ele => f(ele) :: MyNil)

  def foreach(f: A => Unit): Unit = map(f)

  def reverse: MyList[A] = foldLeft(MyNil: MyList[A])((next, head) => head :: next)

  override def toString: String = "MyList(" + foldRight("")((head, string) => {
    if (string.isEmpty) {
      head.toString
    } else {
      head.toString + ", " + string
    }
  } + ")")

}

case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A]

case object MyNil extends MyList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException("head of empty list")

  override def tail: MyList[Nothing] = throw new NoSuchElementException("tail of empty list")

}
