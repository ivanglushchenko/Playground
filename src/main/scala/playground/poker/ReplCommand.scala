package playground.poker

abstract sealed class ReplCommand extends Function[Environment, Option[Environment]]

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
    println("community cards: " + env.openCards)
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
        def toProb(c: Int): Double = (c.toDouble * 10000.0 / count.toDouble).toInt / 100.0
        def toName(i: Int): String = Combination.factories(Combination.factories.length - i - 1).toString

        println("sum = " + count)
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

/**
 * Use this command for testing purposes only
 */
case class TestCommand(hand: Option[Hand]) extends ReplCommand {
  override def apply(env: Environment) = {
    // 3S 4S 5S        c1 = 1081, c2 = 1081, c3 = 1081 / 3991, 3151 !!!
    // 4S 5S 6S        c1 = 1081, c2 = 1081, c3 = 1081 / 3151, 3151
    hand match {
      case Some(h) =>
        val deck = Deck.Full52 - h.cards
        var c1 = 0
        var c2 = 0
        var c3 = 0
        var cStraights = 0
        var cStraightsComb = 0

        Permutations.foreach(deck.cards.toArray, 7 - h.cards.size, 0, deck.cards.size){
          cards => {
            var b = false
            def contains(out: Card) = cards.head == out || cards.tail.head == out || cards.tail.tail.head == out || cards.tail.tail.tail.head == out
            if (contains(Card(Ace, Spades)) && contains(Card(NumRank(2), Spades))){
              c1 += 1
              b = true
              cStraightsComb += 1
            }
            if (contains(Card(NumRank(6), Spades)) && contains(Card(NumRank(7), Spades))) {
              c2 += 1
              if (!b) cStraightsComb += 1
              b = true
            }
            if (contains(Card(NumRank(2), Spades)) && contains(Card(NumRank(6), Spades))) {
              c3 += 1
              if (!b) cStraightsComb += 1
              b = true
            }
            val fullHand = Hand(h.cards ::: cards)
            val best = Combination best fullHand
            best match {
              case StraightFlush(c) =>
                cStraights += 1
                if (!b) println("err? " + c + ", hand " + fullHand)
              case _ =>
            }
          }
        }
        println("c1 = " + c1 + ", c2 = " + c2 + ", c3 = " + c3 + " / " + cStraights + ", " + cStraightsComb)
      case None =>
        val h = Hand parse "3S 4S 5S 2S 8C JS AD" get
        val best = Combination best h
        println(best.toString)
    }
    Some(env)
  }
}

case object ClearCommand extends ReplCommand {
  override def apply(env: Environment) = Some(Environment())
}