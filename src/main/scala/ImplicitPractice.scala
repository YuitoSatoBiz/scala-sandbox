//import implicitPractice.ImplicitSupplier._
import implicitPractice.StringBox

/**
  * Created by satoyuito on 2017/08/06.
  */
object ImplicitPractice {

  def test(): Unit = {
//    val text = "クールっすね"
    //    if (keigoCheck(text)) {
    //      println("Super COOOL!!!")
    //    }

//    println("Good".formalize)
  }

  /**
    * 語尾に「っすね」をつけるだけのメソッド
    * formalEndはStringBoxオブジェクトからimplicitで取得する。
    * endsWithの引数はStringしか受け付けないがStringBoxオブジェクトで暗黙的にStringBoxをStringに変換している
    */
  def keigoCheck(text: String)(implicit formalEnd: StringBox): Boolean = {
    text.endsWith(formalEnd)
  }
}
