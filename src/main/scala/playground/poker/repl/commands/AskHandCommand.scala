package playground.poker.repl.commands

import playground.poker.Environment

case object AskHandCommand extends ReplCommand {
  override def apply(env: Environment) = {
    println("hands: " + env.hands)
    println("community cards: " + env.openCards)
    Some(env)
  }
}
