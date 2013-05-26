package playground.poker.repl.commands

import playground.poker.combinations._
import playground.poker.cards._
import playground.poker.repl._
import playground.poker._
import playground.poker.util.{Permutations, Stopwatch}

case class TestCommand(hand: Option[Hand]) extends ReplCommand {
  override def apply(env: Environment) = {

    val h = ReplInput parseHand "ac qd ah js js" get
    val ff = h.ranks
    val b = Combination best h

    Stopwatch.measure("going through all combs") {
      var c = 0
      Permutations.foreach(Deck.Full52.cards.toArray, 7, 0, 51) { cards => c += 1 }
      println("c = " + c)
    }

    /*
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
    */

    Some(env)
  }
}