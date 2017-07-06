/**
  * Created by satoyuito on 2017/06/19.
  */
object Main {
  def main(args: Array[String]): Unit = {
    new List(1, 2)
    val list: MyList[Int] = 2::MyCons(1, MyNil)
    list.map { a =>
      println(a)
      a
    }
    //TODO  :::がMyConsで取り扱っているため死ぬ
  }
}
