package playground.poker

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
    println("available commands: exit, help, prob ...")
    None
  }
}

case class HandCommand(hand: Hand) extends ReplCommand {
  override def apply(env: Environment) = {
    println("your hand: " + hand + ", best combo: " + hand.combinations.head)
    Some(env add hand)
  }
}

case object ProbabilityCommand extends ReplCommand {
  override def apply(env: Environment) = {
    for { f <- Combination.factories } println("" + f + ": prob " + f.prob(env.myHand) + ", outs " + f.outs(env.myHand))
    Some(env)
  }
}