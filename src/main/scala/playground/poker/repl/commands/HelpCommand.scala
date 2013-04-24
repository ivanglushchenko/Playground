package playground.poker.repl.commands

import playground.poker.Environment

case object HelpCommand extends ReplCommand  {
  override def apply(env: Environment) = {
    println("available commands: [hand], exit, help, prob, + [card], ...")
    Some(env)
  }
}