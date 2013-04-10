package playground.poker

case class Hand(cards: List[Card]) {
  override def toString = cards mkString " "

  val ranks =
    cards
      .groupBy(_.rank).toList.map(p => (p._1, p._2.length))
      .sortWith((r1, r2) => r1._1.compareTo(r2._1) < 0)
      .sortWith((r1, r2) => r1._2.compareTo(r2._2) < 0)
      .reverse
  val suits = cards.groupBy(_.suit).toList.map(p => (p._1, p._2.length)).sortBy(p => p._2).reverse
  val diffs =
    cards
      .map(_.rank.value)
      .sorted
      .foldLeft(List((0, 0)))((acc, n) => (n, n - acc.head._1) :: acc)
      .map(_._2)
      .take(4)
  val highCards = (cards map (card => HighCard(card.rank))).sortBy(_.rank).reverse//.map(_.asInstanceOf[Combination])

  def combinations(): List[Combination] = Combination.best(this) match {
    case Some(c) => c :: highCards
    case None => highCards
    /*
    this match {
      case RoyalFlush(c) => c :: highCards
      case StraightFlush(c) => c :: highCards
      case FourOfKind(c) => c :: highCards
      case FullHouse(c) => c :: highCards
      case Flush(c) => c :: highCards
      case Straight(c) => c :: highCards
      case ThreeOfKind(c) => c :: highCards
      case TwoPairs(c) => c :: highCards
      case OnePair(c) => c :: highCards
      case _ => highCards
    }
    */
  }

  /*
  def bestCombination() = (ranks, suits, diffs) match {
    case (List((Ace, _), _*), List((_, i), _*), List(1, 1, 1, 1)) if i >= 4 => Some(RoyalFlush())
    case (_, List((_, i), _*), List(1, 1, 1, 1)) if i >= 4 => Some(StraightFlush(highCards.head.rank))
    case (List((r, 4), _*), _, _) => Some(FourOfKind(r))
    case (List((r1, 3), (r2, 2), _*), _, _) => Some(FullHouse(r1, r2))
    case (_, List((r, i), _*), _) if i >= 4 => Some(Flush(highCards.head.rank))
    case (_, _, List(1, 1, 1, 1)) => Some(Straight(highCards.head.rank))
    case (List((r, 3), _*), _, _) => Some(ThreeOfKind(r))
    case (List((r1, 2), (r2, 2), _*), _, _) => Some(TwoPairs(r1, r2))
    case (List((r, 2), _*), _, _) => Some(OnePair(r))
    case _ => None
  }
  */
}

object Hand {
  def unapply(s: String): Option[Hand] = {
    ((s split " ").toList) match {
      case Card(c1) :: Card(c2) :: Card(c3) :: Card(c4) :: Card(c5) :: _ => Some(Hand(List(c1, c2, c3, c4, c5)))
      case _ => None
    }
  }
}

object Hands {
  def unapplySeq(s: String): Option[Seq[Hand]] = {
    def getNext(cards: List[String]): List[Hand] = cards match {
      case Card(c1) :: Card(c2) :: Card(c3) :: Card(c4) :: Card(c5) :: tl => Hand(List(c1, c2, c3, c4, c5)) :: getNext(tl)
      case _ => Nil
    }

    getNext((s split " ").toList) match {
      case hd :: nk :: tl => Some(hd :: nk :: tl)
      case _ => None
    }
  }
}
