package playground

import playground.poker._
import org.scalatest.FunSuite
import scala.io._
import java.io._

class PokerSuite extends FunSuite {
  test("test ranks") {
    val hand = Hand.unapply("7D 2S 5D 7S AC").get
    assert(hand.ranks === List((NumRank(7), 2), (Ace, 1), (NumRank(5), 1), (NumRank(2), 1)))
  }

  test("compare hands") {
    val fileName = new File(".").getCanonicalPath() + "\\src\\test\\scala\\playground\\PokerHands.txt"
    val winningHands = Source.fromFile(fileName).getLines().map(_ match {
      case Hands(hand1, hand2, _*) => if (hand1 > hand2) 1 else 0
    }).sum
    assert(winningHands === 376)
  }
}
