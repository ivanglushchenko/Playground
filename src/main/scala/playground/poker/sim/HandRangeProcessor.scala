package playground.poker.sim

import playground.poker.cards._
import playground.poker.combinations.Combination
import playground.poker.util.Permutations
import scala.actors.Actor

class HandRangeProcessor(sender: Actor, myCards: List[Card], remainingCards: Array[Card], k: Int, range: (Int, Int)) extends Actor {
  def act() {
    val combinations = Array.ofDim[Int](Combination.factories.length)
    Permutations.foreach(remainingCards, k, range._1, range._2) {
      cards => {
        val fullHand = Hand(myCards ::: cards)
        val bestComb = Combination.best(fullHand)
        combinations(bestComb.value) += 1
      }
    }
    sender ! MsgHandRangeProcCompleted(combinations)
  }

  start()
}