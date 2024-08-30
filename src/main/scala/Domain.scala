package macrosPlayground

final case class User(id: Int, name: String)

trait Empty[A]:
  val empty: A


given Empty[String] with
  val empty = ""


given Empty[Int] with
  val empty = 0
