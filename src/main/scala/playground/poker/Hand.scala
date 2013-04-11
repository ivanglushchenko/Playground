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
  }
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
