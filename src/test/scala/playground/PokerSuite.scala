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

  test("compare best combinations") {
    def getBestComb(hand: String) = hand match {
        case Hand(hand) => hand.combinations.head
    }

    assert(getBestComb("7D 2S 5D 3S AC") === HighCard(Ace))
    assert(getBestComb("7D 2S 5D 7S AC") === OnePair(NumRank(7)))
    assert(getBestComb("7D 2S 5D 7S 5C") === TwoPairs(NumRank(7), NumRank(5)))
    assert(getBestComb("7D 2S 9D 7S 9C") === TwoPairs(NumRank(9), NumRank(7)))
    assert(getBestComb("7D 2S 5D 7S 7C") === ThreeOfKind(NumRank(7)))
    assert(getBestComb("6D 2S 4D 3S 5C") === Straight(NumRank(6)))
    assert(getBestComb("6D 2D 4D AD 5D") === Flush(Ace))
    assert(getBestComb("7D 2S 2D 7S 2C") === FullHouse(NumRank(2), NumRank(7)))
    assert(getBestComb("2D 2S 2D 7S 2C") === FourOfKind(NumRank(2)))
    assert(getBestComb("6D 7D TD 8D 9D") === StraightFlush(NumRank(10)))
    assert(getBestComb("QD KD TD AD JD") === RoyalFlush())
  }
}
