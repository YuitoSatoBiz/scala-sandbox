package future

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class MyFuture {
  Future {
    Thread.sleep(500)
  }

}

