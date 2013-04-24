package playground.poker.util

object Stopwatch {
  def measure(caption: String)(action: => Unit) {
    val startTime = System.currentTimeMillis
    print(caption + "...")
    action

    val dt = (System.currentTimeMillis - startTime).toDouble / 1000.0
    println("done in " + dt + "s")
  }
}