import java.time.LocalDate

/**
  * Created by satoyuito on 2017/09/15.
  */
object SortPractice {

  def test(): Unit = {
    val today = LocalDate.of(2017, 9, 11)
    println(today)
    println(rangeDays(today, today.plusDays(5))) // =>  List(2017-09-11, 2017-09-12, 2017-09-13, 2017-09-14, 2017-09-15, 2017-09-16)
    println(listDays(today, 3)) // => List(2017-09-11, 2017-09-12, 2017-09-13)
    println(listDaysWithStep(today, 10, 3)) // => List(2017-09-11, 2017-09-14, 2017-09-17, 2017-09-20)
  }

  /** 2つの指定日とその間の日からなるLocalDateのシーケンス（昇順）を返す */
  def rangeDays(from: LocalDate, to: LocalDate): Seq[LocalDate] = {
    val diff = to.toEpochDay - from.toEpochDay
    listDays(from, diff.toInt + 1)
  }

  /** 指定日を含むn日間のLocalDateのシーケンス（昇順）を返す */
  def listDays(from: LocalDate, days: Int): Seq[LocalDate] = {
    (for (i <- 0 until days) yield from.plusDays(i)).toList
  }

  /** ↑にステップを導入したもの */
  def listDaysWithStep(from: LocalDate, days: Int, step: Int): Seq[LocalDate] = {
    (for (i <- 0 to days / step) yield from.plusDays(i * step)).toList
  }
}
