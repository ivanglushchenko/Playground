package playground.poker.repl.commands

import playground.poker._
import playground.poker.combinations.Combination
import playground.poker.sim.Simulator

case object ProbabilityCommand extends ReplCommand {
  override def apply(env: Environment) = {
    val sim = new Simulator(env.myFullHand.cards, env.deck.cards.toArray)
    sim.getCombinations() match {
      case Some(combinations) =>
        val count = combinations.sum
        def toProb(c: Int): Double = (c.toDouble * 10000.0 / count.toDouble).toInt / 100.0
        def toName(i: Int): String = Combination.factories(Combination.factories.length - i - 1).toString

        println("sum = " + count)
        for (t <- 0 until Combination.factories.length)
          println("  " + toName(t) + " -> " + toProb(combinations(t)) + "% (" + combinations(t) + ")")

      case None =>
        println("Unknown result")
    }

    Some(env)
  }
}
