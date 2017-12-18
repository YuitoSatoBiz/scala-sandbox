import java.nio.Buffer

import scala.annotation.tailrec

/**
  * @author yuito.sato
  */
object MyList {

  def apply[A](head: A, tail: MyList[A]): MyList[A] = MyCons(head, tail)

  def test(): Unit = {
//    val list = MyList("1", MyList("2", MyNil))

//    val result = list.foldLeft("0")(_ + _)
//    val result = list.foldRight("0")(_ + _)
//    println(result)



//    val list1 = MyList(1, MyList(2, MyNil))
//    val list2 = MyList(4, MyNil)
//    val list3 = 3::list2
//    val list4 = (list1 ::: list3).map { e =>
//      e + 1
//    }
//    list4.foreach { e =>
//      println(e)
//    }
//    val list = MyList(1, MyList(2, MyNil))
//    val leftResult = list.foldLeft("") { (acc: String, x) => acc + x.toString }

//    println(leftResult)

//    val rightResult = list.foldRight("") { (x, acc) => x.toString + acc }
//    println(rightResult)
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

//  @tailrec
//  def foldRight[B](zero: B)(f: (A, B) => B): B =
//    if (this.isEmpty) zero
//    else f(head, tail.foldRight(zero)(f))

  private def foldLeft[B](zero: B)(f: (B, A) => B): B = {
    def loop(list: MyList[A], acc: B): B = list match {
      case MyNil => acc
      case MyCons(h, t) => loop(t, f(acc, h))
    }
    loop(this, zero)

//    if (this.isEmpty) zero
//    else tail.foldLeft(f(zero, head))(f)
  }

  def foldRight[B](zero: B)(f: (A, B) => B): B = {
    def loop(list: MyList[A], acc: B): B = list match {
      case MyNil => acc
      case MyCons(h, t) => {
//        loop(// なんか)

        // kokomadehakakutei
        loop(t, //)

        loop(t, f(last, acc))
        loop(t, f(h, acc))
        f(h, loop(t, acc))
      }
    }

    loop(this, zero)


//    this.reverse.foldLeft(zero)(f)
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


//  https://dev.classmethod.jp/server-side/scala-foldright-foldleft/
//  @tailrec
//  def foldRight[B](zero: B)(f: (A, B) => B): B = {
//    //    if (this.isEmpty) zero
//    //    else tail.foldRight(f(head, zero))(f)
//
//
//
//    if (this.isEmpty) {
//      zero
//    } else {
//      f(tail.foldRight(zero)(f), head)
//    }
//  }

  // scala fiddle用
//  def reverse[A](l: List[A], zero: List[A]): List[A] = {
//    l.foldLeft(Nil: List[A])({(z: List[A], a: A) => a :: z})
//  }
//  val r = reverse(List(1,2,3,4), Nil)
//  println(r)

  def reduce[A, B](f: (A, B) => B, x: B)(list: MyList[A]): B = list match {
    case MyNil => x
    case MyCons(hd, tl) => f(hd, reduce(f, x)(tl))
  }
    // TODO ループしてる
    // foldLeftも内部関数的に書く
    // rightも頑張って書いてみる

    // foldLeftならできた。
//    foldLeft(MyNil: MyList[A])({(z: MyList[A], a: A)=> a :: z })



//    foldLeft[B](zero: B)(f: (B, A) => B): B =
//    if (this.isEmpty) zero
//    else tail.foldLeft(f(zero, head))(f)


  def add(x: Int, y: Int) = x + y

  // 実践で使うにはどう実装すればいいか？
  def sum: MyList[Int] => Int = reduce[Int, Int](add, 0)

  // オブジェクトメソッドとしては使えるが、インスタンスメソッドとして使えない
//  def copy[A](l: MyList[A]) = reduce(MyCons[A], MyNil)(l)

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
