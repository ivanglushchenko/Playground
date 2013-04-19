package playground.poker

object Stopwatch {
  def measure(caption: String)(action: => Unit) {
    val startTime = System.currentTimeMillis
    print(caption + "...")
    action

    val dt = System.currentTimeMillis - startTime
    println("done in " + dt)
  }
}
