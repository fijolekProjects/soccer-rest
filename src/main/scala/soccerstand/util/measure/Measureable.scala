package soccerstand.util.measure

trait Measureable {
  def measure[T](actionName: String)(block: => T): T = {
    val before = System.currentTimeMillis()
    val retValue = block
    val time = System.currentTimeMillis() - before
    println(s"$actionName took: $time milliseconds.")
    retValue
  }
}