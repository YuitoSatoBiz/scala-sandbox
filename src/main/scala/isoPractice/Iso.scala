package isoPractice


trait Iso[A, B] {

  def to(a: A): B
  def from(b: B): A
}

case class MyId[A](value: Int) extends AnyVal

object MyId {

  implicit def iso[A]: Iso[Int, MyId[A]] = new Iso[Int, MyId[A]] {
    def to(value: Int): MyId[A]  = MyId(value)
    def from(model: MyId[A]): Int = model.value
  }
}
