package playground.poker.cards

import playground.poker.combinations._

case class Hand(val cards: List[Card]) extends Ordered[Hand] {
  override def toString = cards mkString " "

  val sortedCards = cards sortBy (_.rank) reverse

  lazy val ranks =
    cards
      .groupBy(_.rank).toList.map(p => (p._1, p._2.length))
      .sortWith((r1, r2) => r1._1.compareTo(r2._1) < 0)
      .sortWith((r1, r2) => r1._2.compareTo(r2._2) < 0)
      .reverse

  lazy val suits = cards.groupBy(_.suit).toList.map(p => (p._1, p._2.length)).sortBy(-_._2)

  lazy val straight = if (cards.length < 5) None else {
    def isConn(c1: Card, c2: Card) = c1.rank - c2.rank == 1

    def getLongest(prevCard: Card, nextCards: List[Card], acc: List[Card], max: List[Card]): List[Card] = nextCards match {
      case hd :: tl =>
        if (isConn(prevCard, hd)) getLongest(hd, tl, hd :: acc, max)
        else if (prevCard.rank == hd.rank) getLongest(hd, tl, acc, max)
        else getLongest(hd, tl, List(hd), if (max.size < acc.size) acc else max)
      case _ => if (max.size < acc.size) acc else max
    }

    def attachWheel(cards: List[Card], allCards: List[Card]) =
      if (cards.size >= 4 && cards.head.rank == NumRank(2) && allCards.head.rank == Ace) allCards.head :: cards
      else cards

    def getLongestStraightSeq(cards: List[Card]) = {
      val longestSeqWithoutWheel = getLongest(cards.head, cards.tail, List(cards.head), List())
      attachWheel(longestSeqWithoutWheel, cards)
    }

    val longestSeq = getLongestStraightSeq(sortedCards)

    (longestSeq.size, suits.head._2) match {
      case (i, _) if i < 5 => None
      case (_, s) if s < 5 => Some(longestSeq.last.rank, false)
      case _ =>
        val suitedCards = sortedCards.filter(_.suit == suits.head._1)
        val straightFlush = getLongestStraightSeq(suitedCards)
        if (straightFlush.size < 5) Some(longestSeq.last.rank, false)
        else Some(straightFlush.last.rank, true)
    }
  }

  lazy val highCards = (cards map (card => HighCard(card.rank))) sortBy (_.rank) reverse
  lazy val combinations: List[Combination] = Combination.best(this) :: highCards

  def compare(that: Hand): Int = {
    def compareCombinations(c1: List[Combination], c2: List[Combination]): Int = (c1, c2) match {
      case (hd1 :: tl1, hd2 :: tl2) => hd1.compare(hd2) match {
        case 0 => compareCombinations(tl1, tl2)
        case i => i
      }
      case (_, _) => 0
    }
    compareCombinations(combinations, that.combinations)
  }
}