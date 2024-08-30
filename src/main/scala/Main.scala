package macrosPlayground

@main def testing() =
  val user = User(3, "Some name")

  val clearedId = clearFld(user, _.id)
  assert(clearedId == User(0, "Some name"))

  val clearedName = clearFld(user, _.name)
  assert(clearedName == User(3, ""))

  println("Full: " + user)
  println("No id: " + clearedId)
  println("No name: " + clearedName)
