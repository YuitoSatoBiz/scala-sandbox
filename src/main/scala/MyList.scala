import java.nio.Buffer

import scala.annotation.tailrec

/**
  * @author yuito.sato
  */
object MyList {

  def apply[A](head: A, tail: MyList[A]): MyList[A] = MyCons(head, tail)

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
    def f(zero: MyList[A], list: MyList[A]): MyList[A] = {
      if (list.isEmpty) {
        println(zero)
        zero
      } else {
        println(tail)
        f(list.head :: zero, list.tail)
      }
    }

    f(MyNil, this)
  }

  def reduce[A, B](f: (A, B) => B, x: B)(list: MyList[A]): B = list match {
    case MyNil => x
    case MyCons(hd, tl) => f(hd, reduce(f, x)(tl))
  }


  def add(x: Int, y: Int) = x + y

  // 実践で使うにはどう実装すればいいか？
  def sum: MyList[Int] => Int = reduce[Int, Int](add, 0)

  def copy = reduce(MyCons[A], MyNil)(this)

  def double(n: Int) = 2 * n

  def fAndCons[A, B](f: A => B)(el: A, list: MyList[B]) = MyCons(f(el), list) // 2 * num => f(el)

  def doubleAndCons: (Int, MyList[Int]) => MyList[Int] = fAndCons(double)
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
