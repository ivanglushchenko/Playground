package playground.poker

trait Environment {
  val hands: Map[String, Hand]
  def add(hand: Hand): Environment

  def myHand() = hands("")
}

object Environment {
  def apply(): Environment = new EnvironmentImpl(Map.empty)

  private class EnvironmentImpl(val hands: Map[String, Hand]) extends Environment {
    def add(hand: Hand): Environment = new EnvironmentImpl(hands + ("" -> hand))
  }
}

