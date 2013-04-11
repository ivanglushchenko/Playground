package main.scala.playground.poker

object TwoHalfs {
  def unapply(s: String): Option[(String, String)] = Some(s.substring(0, s.size / 2), s.substring(s.size / 2 + 1))
}
