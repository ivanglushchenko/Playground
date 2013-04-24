package playground.poker.repl.commands

import playground.poker.combinations._
import playground.poker.cards._
import playground.poker.repl._
import playground.poker._

case class TestCommand(hand: Option[Hand]) extends ReplCommand {
  override def apply(env: Environment) = {
    hand match {
      case Some(h) =>
        val best = Combination best h
        println(best.toString)
      case None =>
        val h = ReplInput parseHand "3S 4S 5S 2S 8C JS AD" get
        val best = Combination best h
        println(best.toString)
    }
    Some(env)
  }
}