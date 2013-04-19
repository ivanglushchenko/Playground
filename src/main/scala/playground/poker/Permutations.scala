package playground.poker

object Permutations {
  /**
   * This function is supposed to be as simple and as fast as possible - we need every bit of
   * raw performance while iterating through all combinations
   */
  def foreach(cards: Array[Card], k: Int)(proc: List[Card] => Unit): Unit = k match {
    case 5 =>
      for {
        i <- 0 until cards.length - 4
        j <- i + 1 until cards.length - 3
        k <- j + 1 until cards.length - 2
        l <- k + 1 until cards.length - 1
        m <- l + 1 until cards.length
      //
      } proc(List(cards(i), cards(j), cards(k), cards(l), cards(m)))
    case _ => throw new Exception
  }
}
