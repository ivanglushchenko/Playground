package playground.poker.repl.commands

import playground.poker.cards.Hand
import playground.poker.Environment

case class SetHandCommand(name: String, hand: Hand) extends ReplCommand {
  override def apply(env: Environment) = {
    val newEnv = env + (name -> hand)
    println("hands: " + newEnv.hands.map(_._2).mkString(", "))
    Some(newEnv)
  }
}
