package playground.poker

trait Combination extends Ordered[Combination] {
  val value: Int
  type CombinationType <: Combination
  def compareWithSameKind(c: CombinationType): Int

  def compare(that: Combination) =
    if (value == that.value) compareWithSameKind(that.asInstanceOf[CombinationType])
    else value.compare(that.value)

  def compareRanks(ranks: List[(Rank, Rank)]): Int = ranks match {
    case hd :: tl => hd._1.compare(hd._2) match {
      case 0 => compareRanks(tl)
      case i => i
    }
    case _ => 0
  }
}

trait CombinationFactory {
  def get(hand: Hand): Option[Combination]
}

object Combination {
  val extractors = List(RoyalFlush, StraightFlush, FourOfKind, FullHouse, Flush, Straight, ThreeOfKind, TwoPairs, OnePair)

  def best(hand: Hand) = {
    def loop(list: List[CombinationFactory]): Option[Combination] = list match {
      case hd :: tl => hd.get(hand) match {
        case Some(c) => Some(c)
        case None => loop(tl)
      }
      case Nil => None
    }
    loop(extractors)
  }
}

case class HighCard(rank: Rank) extends Combination {
  override def toString = "High " + rank

  val value = 0
  type CombinationType = HighCard
  def compareWithSameKind(c: HighCard): Int = rank.compare(c.rank)
}

case class OnePair(rank: Rank) extends Combination {
  override def toString = "Pair of " + rank

  val value = 1
  type CombinationType = OnePair
  def compareWithSameKind(c: OnePair): Int = rank.compare(c.rank)
}

object OnePair extends CombinationFactory {
  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r, 2), _*) => Some(OnePair(r))
    case _ => None
  }
}

case class TwoPairs(rank1: Rank, rank2: Rank) extends Combination {
  override def toString = "Two pairs " + rank1 + " and " + rank2

  val value = 2
  type CombinationType = TwoPairs
  def compareWithSameKind(c: TwoPairs): Int = compareRanks(List((rank1, c.rank1), (rank2, c.rank2)))
}

object TwoPairs extends CombinationFactory {
  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r1, 2), (r2, 2), _*) => Some(TwoPairs(r1, r2))
    case _ => None
  }
}

case class ThreeOfKind(rank: Rank) extends Combination {
  override def toString = "Three " + rank

  val value = 3
  type CombinationType = ThreeOfKind
  def compareWithSameKind(c: ThreeOfKind): Int = rank.compare(c.rank)
}

object ThreeOfKind extends CombinationFactory {
  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r, 3), _*) => Some(ThreeOfKind(r))
    case _ => None
  }
}

case class Straight(rank: Rank) extends Combination {
  override def toString = "Straight " + rank

  val value = 4
  type CombinationType = Straight
  def compareWithSameKind(c: Straight): Int = rank.compare(c.rank)
}

object Straight extends CombinationFactory {
  def get(hand: Hand): Option[Combination] = hand.diffs match {
    case List(1, 1, 1, 1) => Some(Straight(hand.highCards.head.rank))
    case _ => None
  }
}

case class Flush(rank: Rank) extends Combination {
  override def toString = "Flush" + rank

  val value = 5
  type CombinationType = Flush
  def compareWithSameKind(c: Flush): Int = rank.compare(c.rank)
}

case object Flush extends CombinationFactory {
  def get(hand: Hand): Option[Combination] = hand.suits match {
    case List((r, i), _*) if i >= 5 => Some(Flush(hand.highCards.head.rank))
    case _ => None
  }
}

case class FullHouse(rank1: Rank, rank2: Rank) extends Combination {
  override def toString = "Full house with " + rank1 + " and " + rank2

  val value = 6
  type CombinationType = FullHouse
  def compareWithSameKind(c: FullHouse): Int = compareRanks(List((rank1, c.rank1), (rank2, c.rank2)))
}

case object FullHouse extends CombinationFactory {
  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r1, 3), (r2, 2), _*) => Some(FullHouse(r1, r2))
    case _ => None
  }
}

case class FourOfKind(rank: Rank) extends Combination {
  override def toString = "Four " + rank

  val value = 7
  type CombinationType = FourOfKind
  def compareWithSameKind(c: FourOfKind): Int = rank.compare(c.rank)
}

object FourOfKind extends CombinationFactory {
  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r, 4), _*) => Some(FourOfKind(r))
    case _ => None
  }
}

case class StraightFlush(rank: Rank) extends Combination {
  override def toString = "Straight flush " + rank

  val value = 8
  type CombinationType = StraightFlush
  def compareWithSameKind(c: StraightFlush): Int = rank.compare(c.rank)
}

object StraightFlush extends CombinationFactory {
  def get(hand: Hand): Option[Combination] = (hand.suits, hand.diffs) match {
    case (List((r, i), _*), List(1, 1, 1, 1)) if i >= 5 => Some(StraightFlush(hand.highCards.head.rank))
    case _ => None
  }
}

case class RoyalFlush() extends Combination {
  override def toString = "Royal flush"

  val value = 9
  type CombinationType = RoyalFlush
  def compareWithSameKind(c: RoyalFlush): Int = 0
}

object RoyalFlush extends CombinationFactory {
  def get(hand: Hand): Option[Combination] = (hand.ranks, hand.suits, hand.diffs) match {
    case (List((Ace, _), _*), List((_, i), _*), List(1, 1, 1, 1)) if i >= 5 => Some(RoyalFlush())
    case _ => None
  }
}