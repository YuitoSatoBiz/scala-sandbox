import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{ Failure, Success }

/**
  * Created by satoyuito on 2017/08/06.
  */
object FuturePractice {

  def test(): Unit = {
    mapTest()
    foldLeftTest()
  }

  /**
    * mapブロック内のFutureは並列処理してもオーケー
    * 【結果】
    * 3
    * 4
    * Future(<not completed>) <- ここだけ終わらない。リストの順番そのものは変更なし
    * Future(Success(3))
    * Future(Success(4))
    * 2
    */
  def mapTest(): Unit = {
    val ints = List(1, 2, 3)
    val secs = List(7, 1, 2)
    def result = ints.map { i =>
      Future {
        val sec = secs(i - 1)
        Thread.sleep(1000 * sec)
        val res = i + 1
        if (i == 2) {
          throw new Exception("え？")
        }
        println(res)
        res
      }
    }
    val f = Future.sequence(result)
    f.onComplete{
      case Success(resultInts) => println(resultInts.mkString)
      case Failure(e) => println(e.getMessage)
    }
    Await.ready(f, Duration.Inf)




    //    val f = Future.sequence(result)
//    f.onSuccess{ case result => println(result.mkString) }


//    ints.map { i =>
//      Future {
//        val sec = secs(i - 1)
//        Thread.sleep(1000 * sec)
//        val res = i + 1
//        println(res)
//        res
//      }
//    }
//    result.foreach(i => println(i))
  }

  /**
    * foldLeftは一つ前の処理が終わらないと、次の処理に行けないのでブロック内で非同期の処理があっても直列的に処理される。というかAwaitを使わざるえない
    * 【結果】
    * 1
    * 3
    * 6
    */
  def foldLeftTest(): Unit = {
    val ints = List(1, 2, 3)
    val secs = List(7, 1, 2)
    ints.foldLeft(0) { (j, i) =>
      val future = Future {
        val sec = secs(i - 1)
        Thread.sleep(1000 * sec)
        val res = j + i
        println(res)
        res
      }
      Await.result(future, 9.seconds)
    }
  }
}
