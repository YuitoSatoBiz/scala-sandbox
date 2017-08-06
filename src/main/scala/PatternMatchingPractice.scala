import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by satoyuito on 2017/08/07.
  */
object PatternMatchingPractice {

  def test(): Unit = {
    val fruits = List(Some("apple"), None, Some("lemon"))
    val futureFtuits = for {
      List(Some(a), Some(b), Some(c)) <- Future.successful(fruits)
    } yield List(a, b, c)
    futureFtuits.map { f =>
      println(f)
    }.recover{ case _ =>
      println("ミスですよ")
    }
  }
}
