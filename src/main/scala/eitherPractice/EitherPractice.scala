package eitherPractice

case class User(email: String, password: String)

object UserService {
  def signUp(email: String, password: String): Either[PasswordError, User] = {
    if (password.length > 50) {
      Left(TooLongPasswordError)
    } else if (password.length < 10) {
      Left(TooShortPasswordError)
    } else {
      Right(User(email, password))
    }
  }
}

object UserController {
  def signUp(email: String, password: String): Unit = {
    val result = UserService.signUp(email, password)
    result match {
      case Right(user) => println(s"${user.email}")
      case Left(TooLongPasswordError) => println(TooLongPasswordError.message)
      case Left(TooShortPasswordError) => println(TooShortPasswordError.message)
    }
  }
}

sealed trait PasswordError {
  val code: String
  val message: String
}

case object TooLongPasswordError extends PasswordError {
  val code = "error.tooLongPassword"
  val message = "Too Long!"
}

case object TooShortPasswordError extends PasswordError {
  val code = "error.tooShortPassword"
  val message = "Too Short!"
}


//sealed trait LoginError {
//  val message: String
//}
//// パスワードが間違っている場合のエラー
//case object InvalidPassword extends LoginError {
//  val message = "Invalid Password"
//}
//// nameで指定されたユーザーが見つからない場合のエラー
//case object UserNotFound extends LoginError {
//  val message = "User Not Found"
//}
//// パスワードがロックされている場合のエラー
//case object PasswordLocked extends LoginError {
//  val message = "Password Locked"
//}
