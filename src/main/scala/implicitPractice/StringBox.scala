package implicitPractice

/**
  * Created by satoyuito on 2017/08/06.
  */
case class StringBox(value: String)

/**
  * StringBoxの暗黙的型パラメーターと型変換メソッドを用意
  */
object StringBox {

  implicit val implicitFormalEnd = StringBox("っすね")

  implicit def stringBoxToString(stringBox: StringBox): String = stringBox.value
}
