package playground

import playground.poker._

object Program extends App {
  def toHand(s: String): Hand = s match {
    case Hand(hand) => hand
  }

  def printComb(hand: String) {
    hand match {
      case Hand(hand) => println(hand + ": " + hand.combinations.mkString(", ") + ",      " + hand.ranks)
      case _ => println("?")
    }
  }

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

  //"7D 2S 5D 3S AC" match {// 8C TS KC 9H 4S" match {
  //  case Hands(hand1, hand2, _*) => println("compare hands " + hand1 + " and " + hand2)
  //  case Hand(hand) => println("combinations for " + hand.combinations.mkString(", "))
  //  case _ => println("?")
  //}
}