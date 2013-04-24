package playground.poker.repl.commands

import playground.poker.Environment

case object ExitCommand extends ReplCommand {
  override def apply(env: Environment) = {
    println("so long and thanks for all the fish")
    None
  }
}
