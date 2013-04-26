package playground.poker.repl.commands

import playground.poker.cards._
import playground.poker.combinations.Combination
import playground.poker.Environment
import playground.poker.util.Permutations

/**
 * Should you call a bet?
 */
case class CallCommand(bet: Int, callers: Option[Int]) extends ReplCommand {
  override def apply(env: Environment) = {
    if (env.openCards.size < 3) println("It's hard to say before the flop")
    else {
      val pot = env.pot + bet * (callers.getOrElse(1) + 1)
      val odds = (bet.toDouble * 100 / pot).toInt
      val outs = getOuts(env) groupBy (_._1) map (g => (g._1, g._2.size)) toList
      val sum = outs.map(_._2).sum

      println("pot%: " + odds + "%")
      println("outs: " + (sum * 100.0 / env.deck.cards.size).toInt + "% (" + sum + ")")
      for ((comb, num) <- outs.sorted)
        println("  " + comb + ": " + num)
    }
    Some(env)
  }

  def getOuts(env: Environment): List[(Combination, Card)] = {
    for {
      cards <- Permutations(env.deck.cards, 1)
      card = cards.head
      bestComb = Combination.best(Hand(cards ::: env.myFullHand.cards))
      if (bestComb.value >= 4)
    } yield (bestComb, card)
  }
}