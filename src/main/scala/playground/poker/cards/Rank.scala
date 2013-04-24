package playground.poker.cards

abstract sealed class Rank extends Ordered[Rank] {
  val value: Int

  def compare(that: Rank) = value compare that.value
}

case object Ace extends Rank {
  val value = 14
  override def toString = "A"
}

case object King extends Rank {
  val value = 13
  override def toString = "K"
}

case object Queen extends Rank {
  val value = 12
  override def toString = "Q"
}

case object Jack extends Rank {
  val value = 11
  override def toString = "J"
}

case class NumRank(value: Int) extends Rank {
  override def toString = if (value == 10) "T" else value.toString
}

object Rank {
  implicit def rankToInt(rank: Rank): Int = rank.value

  val All = (for (i <- 2 to 10) yield NumRank(i)).toArray ++ Array(Jack, Queen, King, Ace)
}
