package playground.poker

import scala.actors.Actor

abstract sealed class ReplCommand {
  def apply(env: Environment): Option[Environment] = Some(env)
}

case object ExitCommand extends ReplCommand {
  override def apply(env: Environment) = {
    println("so long and thanks for all the fish")
    None
  }
}

case object HelpCommand extends ReplCommand  {
  override def apply(env: Environment) = {
    println("available commands: [hand], exit, help, prob, + [card], ...")
    None
  }
}

case class AddHandCommand(name: String, hand: Hand) extends ReplCommand {
  override def apply(env: Environment) = {
    val newEnv = env + (name -> hand)
    println("hands: " + newEnv.hands)
    Some(newEnv)
  }
}

case object AskHandCommand extends ReplCommand {
  override def apply(env: Environment) = {
    println("raw hands: " + env.hands)
    println("ext hands: " + env.extHands())
    Some(env)
  }
}

case object ProbabilityCommand extends ReplCommand {
  override def apply(env: Environment) = {
    val combinations: Array[Int] = Array.ofDim(Combination.factories.length)

    val dispatcher = new Dispatcher(env.myHand.cards, env.deck.cards.toArray, 1)

    Stopwatch.measure("enum hands") {
      Permutations.foreach(env.deck.cards.toArray, 5) {
        cards => {
          val fullHand = Hand(env.myHand.cards ::: cards)
          //val bestComb = Combination.best(fullHand)
          //combinations(bestComb.value) = combinations(bestComb.value) + 1
        }
      }
    }

    val count = combinations.sum
    def toProb(c: Int): Double = (c.toDouble * 10000.0 / count.toDouble).toInt / 100.0
    def toName(i: Int): String = Combination.factories(Combination.factories.length - i - 1).toString
    for (t <- 0 until Combination.factories.length)
      println("  " + toName(t) + " -> " + toProb(combinations(t)) + "%")

    Some(env)
  }
}

case class AddOpenCardCommand(card: Card) extends ReplCommand {
  override def apply(env: Environment) = {
    val newEnv = env + card
    println("open cards: " + newEnv.openCards)
    Some(newEnv)
  }
}