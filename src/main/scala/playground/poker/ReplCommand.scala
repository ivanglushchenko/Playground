package playground.poker

abstract sealed class ReplCommand {
  def apply(env: Environment): Option[Environment] = Some(env)
}

case class MultiCommand(commands: List[ReplCommand]) extends ReplCommand {
  override def apply(env: Environment) = {
    def execNext(commands: List[ReplCommand], env: Environment): Option[Environment] = commands match {
      case hd :: tl => hd(env) match {
        case Some(newEnv) => execNext(tl, newEnv)
        case None => None
      }
      case _ => Some(env)
    }
    execNext(commands, env)
  }
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

case class SetHandCommand(name: String, hand: Hand) extends ReplCommand {
  override def apply(env: Environment) = {
    val newEnv = env + (name -> hand)
    println("hands: " + newEnv.hands.map(_._2).mkString(", "))
    Some(newEnv)
  }
}

case object AskHandCommand extends ReplCommand {
  override def apply(env: Environment) = {
    println("hands: " + env.hands)
    println("community cards: " + env.extHands())
    Some(env)
  }
}

case object ProbabilityCommand extends ReplCommand {
  override def apply(env: Environment) = {
    val sim = new Simulator(env.myFullHand.cards, env.deck.cards.toArray)
    sim.start()
    sim !? MsgAllDone match {
      case combinations: Array[Int] =>
        val count = combinations.sum
        println("sum = " + count)
        def toProb(c: Int): Double = (c.toDouble * 10000.0 / count.toDouble).toInt / 100.0
        def toName(i: Int): String = Combination.factories(Combination.factories.length - i - 1).toString
        for (t <- 0 until Combination.factories.length)
          println("  " + toName(t) + " -> " + toProb(combinations(t)) + "% (" + combinations(t) + ")")
      case _ =>
        println("Unknown result")
    }

    Some(env)
  }
}

case class AddCardsCommand(cards: List[Card]) extends ReplCommand {
  override def apply(env: Environment) = {
    val newEnv = env + cards
    println("open cards: " + newEnv.openCards)
    Some(newEnv)
  }
}

case object TestCommand extends ReplCommand {
  override def apply(env: Environment) = {
    def getBestComb(str: String) = Hand parse str match {
      case Some(hand) => hand.combinations.head
      case None =>
        println("failed to parse str " + str)
        throw new Exception
    }
    val hand = Hand parse "ad kd qd jd jc td" get
    val straight = hand.straight
    val best = Combination best hand

    var count = 0
    Permutations.foreach(Deck.Full52.cards.toArray, 7, 0, 46) {
      _ => count += 1
    }

    Some(env)
  }
}

case object ClearCommand extends ReplCommand {
  override def apply(env: Environment) = Some(Environment())
}