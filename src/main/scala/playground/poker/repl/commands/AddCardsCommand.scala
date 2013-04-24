package playground.poker.repl.commands

import playground.poker.cards.Card
import playground.poker.Environment

case class AddCardsCommand(cards: List[Card]) extends ReplCommand {
  override def apply(env: Environment) = {
    val newEnv = env + cards
    println("open cards: " + newEnv.openCards)
    Some(newEnv)
  }
}
