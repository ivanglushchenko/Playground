package playground.poker.repl.commands

import playground.poker.Environment

case class SetPotCommand(pot: Int) extends ReplCommand {
  override def apply(env: Environment) = {
    val newEnv = env + (pot - env.pot)
    println("pot: " + newEnv.pot)
    Some(newEnv)
  }
}