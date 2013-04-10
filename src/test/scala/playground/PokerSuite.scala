package playground

import playground.poker._
import org.scalatest.FunSuite

class PokerSuite extends FunSuite {
  test("aaa") {
    val hand = Hand.unapply("7D 2S 5D 7S AC").get
    assert(hand.ranks == List((NumRank(7), 2), (Ace, 1), (NumRank(2), 1), (NumRank(5), 1)))
  }
}
