import scala.collection.mutable.ListBuffer

/**
  * Created by satoyuito on 2017/06/19.
  */
object Main {

  def main(args: Array[String]): Unit = {
<<<<<<< HEAD
    SortPractice.test()
=======
    val list1 = MyList(1, MyList(2, MyNil))
    val list2 = MyList(4, MyNil)
    val list3 = 3::list2
    val list4 = (list1 ::: list3).map { e =>
      e + 1
    }
    list4.foreach { e =>
      println(e)
    }

    ListBuffer
>>>>>>> 946aeafda1ace0cf4e0a7fca5137f49e01d842bf
  }
}
