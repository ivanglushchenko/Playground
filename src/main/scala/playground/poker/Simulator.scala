package playground.poker

import scala.actors.Actor

case class MsgCalcCompleted(combinations: Array[Int])
case object MsgAllDone

object RangeSplit extends Enumeration {
  type RangeSplit = Value
  val One, Two, Four, Eight = Value
}

import RangeSplit._

class Simulator(myCards: List[Card], remainingCards: Array[Card]) extends Actor {
  def act() {
    val combinations = Array.ofDim[Int](Combination.factories.length)
    val cardsToDraw = 7 - myCards.length
    val ranges = toRanges(Four, remainingCards.length - cardsToDraw + 1)

    Stopwatch.measure("sim hands for " + myCards.mkString(" ")) {
      for (range <- ranges) new Worker(this, cardsToDraw, range)

      for (_ <- 1 to ranges.length) {
        receive {
          case MsgCalcCompleted(c) =>
            for (i <- 0 until Combination.factories.length)
              combinations(i) += c(i)
        }
      }
    }

    receive {
      case MsgAllDone => reply(combinations)
    }
  }

  def toRanges(s: RangeSplit, max: Int) = s match {
    case One => List((0, max))
    case Two => List((0, 6), (6, max))
    case Four => List((0, 3), (3, 7), (7, 12), (12, max))
    case Eight => List((0, 1), (1, 2), (2, 4), (4, 6), (6, 8), (8, 11), (11, 16), (16, max))
  }

  class Worker(sender: Actor, k: Int, range: (Int, Int)) extends Actor {
    def act() {
      val combinations = Array.ofDim[Int](Combination.factories.length)
      Permutations.foreach(remainingCards, k, range._1, range._2) {
        cards => {
          val fullHand = Hand(myCards ::: cards)
          val bestComb = Combination.best(fullHand)
          combinations(bestComb.value) += 1
        }
      }
      //println("done for " + range + ", " + combinations.sum)
      sender ! MsgCalcCompleted(combinations)
    }

    start()
  }
}