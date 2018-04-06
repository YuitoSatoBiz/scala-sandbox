import scala.annotation.tailrec
import scala.util.control.TailCalls._

/**
  * @author yuito.sato
  */
object MyList {

  def apply[A](head: A, tail: MyList[A]): MyList[A] = MyCons(head, tail)

  def apply[A](elements: A*): MyList[A] = {

    def loop(eles: A*):TailRec[MyList[A]] = {
      if (eles.isEmpty) done(MyNil)
      else tailcall { loop(eles.tail: _*) }.map { tail => apply(eles(0), tail)}
    }
    loop(elements: _*).result
  }

  def test(): Unit = {}
}

sealed trait MyList[+A] {

  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def map[B](f: A => B): MyList[B]

  def foreach(f: A => Unit): Unit
  
  def ::[C >: A](x: C): MyList[C]

  def :::[C >: A](prefix: MyList[C]): MyList[C]

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

  def reverse: MyList[A] = {
    @tailrec
    def loop(zero: MyList[A], list: MyList[A]): MyList[A] = list match {
      case MyNil => zero
      case MyCons(h, t) => loop(h :: zero, t)
    }
    loop(MyNil, this)
  }

  def add(x: Int, y: Int): Int = x + y

  def double(n: Int): Int = 2 * n

  def fAndCons[A, B](f: A => B)(el: A, list: MyList[B]) = MyCons(f(el), list) // 2 * num => f(el)

  def doubleAndCons: (Int, MyList[Int]) => MyList[Int] = fAndCons(double)
}

case class MyCons[A](hd: A, tl: MyList[A]) extends MyList[A] {

  override def head: A = hd

  override def tail: MyList[A] = tl

  override def isEmpty: Boolean = false

  override def map[B](f: (A) => B): MyList[B] = MyCons(f(head), tail.map(f))

  override def foreach(f: (A) => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def ::[C >: A](x: C): MyList[C] = MyCons(x, this)

  override def :::[C >: A](prefix: MyList[C]): MyList[C] = {
//    def loop(list: MyCons[A], prefix: MyList[C]) = prefix match {
//      case MyNil => this
//      case MyCons(h, t) => h :: t ::: this
//    }
    prefix match {
      case MyNil => this
      case MyCons(h, t) => h :: t ::: this
      //    if (prefix.isEmpty) {
      //      this
      //    } else {
      //      prefix.head :: prefix.tail ::: this
      //    }
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
