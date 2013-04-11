package playground

import playground.poker._
import java.io.File
import scala.io.Source

object Program extends App {
  def toHand(s: String): Hand = s match {
    case Hand(hand) => hand
  }

  def printComb(hand: String) {
    hand match {
      case Hand(hand) => println(hand + ": " + hand.combinations.mkString(", "))
      case _ => println("?")
    }
  }
/*
  printComb("7D 2S 5D 3S AC")
  printComb("7D 2S 5D 7S AC")
  printComb("7D 2S 5D 7S 5C")
  printComb("7D 2S 9D 7S 9C")
  printComb("7D 2S 5D 7S 7C")
  printComb("6D 2S 4D 3S 5C")
  printComb("6D 2D 4D AD 5D")
  printComb("7D 2S 2D 7S 2C")
  printComb("2D 2S 2D 7S 2C")
  printComb("6D 7D TD 8D 9D")
  printComb("QD KD TD AD JD")

  val l: List[Combination] = List(OnePair(NumRank(8)), HighCard(NumRank(7)).asInstanceOf[Combination], OnePair(NumRank(5)), HighCard(NumRank(5)), OnePair(NumRank(2))).sorted
  println(l)

  "7D 2S 5D 3S AC 8C TS KC 9H 4S" match {
    case Hands(hand1, hand2, _*) => println("compare hands " + hand1 + " and " + hand2 + ", cmp: " + hand1.compare(hand2))
    case Hand(hand) => println("combinations for " + hand.combinations.mkString(", "))
    case _ => println("?")
  }
*/

  def isWinningHand(s: String) = s match { case Hands(hand1, hand2, _*) => {
    println("hd1: " + hand1.combinations + ",    hand2: " + hand2.combinations)
    if (hand1 > hand2) 1 else 0} }


  val fileName = new File(".").getCanonicalPath() + "\\src\\test\\scala\\playground\\PokerHands.txt"
  val lines = Source.fromFile(fileName).getLines()
  val results = Source.fromFile(new File(".").getCanonicalPath() + "\\src\\test\\scala\\playground\\out").getLines().map(_.toInt)
  val winningHands = lines.map(_ match {
    case Hands(hand1, hand2, _*) => {
      //if (hand1 > hand2) 1 else 0
      (if (hand1 > hand2) 1 else 0, hand1, hand2)
    }
  })
  val t = winningHands.zip(results).filter(t => t._1._1 != t._2).toList

  println(t)

  //println(winningHands.sum)
  //println(results.sum)

  //println(isWinningHand("5C 7H 5D KD 9H 4D 3D 2D KS AD"))
  //println(isWinningHand("5D 8C 9S JS AC 2C 5C 7D 8S QH"))
  //println(isWinningHand("2D 9C AS AH AC 3D 6D 7D TD QD"))
  //println(isWinningHand("4D 6S 9H QH QC 3D 6D 7H QD QS"))
  //println(isWinningHand("2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"))
}