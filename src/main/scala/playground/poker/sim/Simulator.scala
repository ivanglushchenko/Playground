package playground.poker.sim

import playground.poker.cards.Card
import playground.poker.combinations.Combination
import playground.poker.util.Stopwatch
import scala.actors.Actor

class Simulator(myCards: List[Card], remainingCards: Array[Card]) extends Actor {
  def act() {
    val combinations = Array.ofDim[Int](Combination.factories.length)
    val cardsToDraw = 7 - myCards.length
    val ranges = RangeSplit.toRanges(RangeSplit.Eight, remainingCards.length - cardsToDraw + 1)

    Stopwatch.measure("sim hands for " + myCards.mkString(" ")) {
      for (range <- ranges) new HandRangeProcessor(this, myCards, remainingCards, cardsToDraw, range)

      for (_ <- 1 to ranges.length) {
        receive {
          case MsgHandRangeProcCompleted(c) =>
            for (i <- 0 until Combination.factories.length)
              combinations(i) += c(i)
        }
      }
    }

    receive {
      case MsgSimCompleted => reply(combinations)
    }
  }

  def getCombinations(): Option[Array[Int]] = {
    start()
    this !? MsgSimCompleted match {
      case combinations: Array[Int] => Some(combinations)
      case _ => None
    }
  }
}
