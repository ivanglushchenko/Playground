package playground.poker

case class Hand(val cards: List[Card]) extends Ordered[Hand] {
  override def toString = cards mkString " "

  lazy val ranks =
    cards
      .groupBy(_.rank).toList.map(p => (p._1, p._2.length))
      .sortWith((r1, r2) => r1._1.compareTo(r2._1) < 0)
      .sortWith((r1, r2) => r1._2.compareTo(r2._2) < 0)
      .reverse

  lazy val suits = cards.groupBy(_.suit).toList.map(p => (p._1, p._2.length)).sortBy(p => p._2).reverse

  lazy val straight = if (cards.length < 5) None else {
    val sortedCards = cards.sortBy(_.rank).reverse

    def isConn(c1: Card, c2: Card) = c1.rank - c2.rank == 1

    val startPoint =
      if (!isConn(sortedCards.tail.head, sortedCards.tail.tail.head)) sortedCards.tail.tail
      else if (!isConn(sortedCards.head, sortedCards.tail.head)) sortedCards.tail
      else sortedCards

    def getLongest(prevCard: Card, nextCards: List[Card], acc: List[Card]): List[Card] = nextCards match {
      case hd :: tl => if (isConn(prevCard, hd)) getLongest(hd, tl, hd :: acc) else acc
      case _ => acc
    }

    val longestSeq = getLongest(startPoint.head, startPoint.tail, List(startPoint.head))
    if (longestSeq.size < 5) None
    else {
      val sortedLongestSeq = longestSeq.reverse
      def getFlush(prevCard: Card, nextCards: List[Card], length: Int, highest: Rank): Option[Rank] = nextCards match {
        case hd :: tl if prevCard.suit == hd.suit =>
          getFlush(hd, tl, length + 1, if (length == 0) prevCard.rank else highest)
        case hd :: tl => getFlush(hd, tl, 0, highest)
        case _ => if (length >= 5) Some(highest) else None
      }
      getFlush(sortedLongestSeq.head, sortedLongestSeq.tail, 1, sortedLongestSeq.head.rank) match {
        case Some(r) => Some(Straight(r), true)
        case None => Some(Straight(sortedLongestSeq.head.rank), false)
      }
    }
  }

  lazy val highCards = (cards map (card => HighCard(card.rank))).sortBy(_.rank).reverse
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

object Hand {
  def parse(str: String) = ReplInput parseHand str
  //: Option[Hand] = {
  //  ((s split " ").toList) match {
  //    case Card(c1) :: Card(c2) :: Card(c3) :: Card(c4) :: Card(c5) :: _ => Some(new Hand(List(c1, c2, c3, c4, c5)))
  //    case _ => None
  //  }
  //}
}

object Hands {
  def parse(str: String): Option[List[Hand]] = {
    Hand parse str match {
      case Some(h) => Some(List(Hand(h.cards take 5), Hand(h.cards drop 5)))
      case None => None
    }
  }
    /*
    def getNext(cards: List[String]): List[Hand] = {
    }
  }

      cards match {
      case Card(c1) :: Card(c2) :: Card(c3) :: Card(c4) :: Card(c5) :: tl => new Hand(List(c1, c2, c3, c4, c5)) :: getNext(tl)
      case _ => Nil
    }

    getNext((s split " ").toList) match {
      case hd :: nk :: tl => Some(hd :: nk :: tl)
      case _ => None
    }

  }
  */
}
