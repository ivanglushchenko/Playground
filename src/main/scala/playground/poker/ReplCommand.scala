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
    for { f <- Combination.factories.drop(8) } println("" + f + ": prob " + f.myProb(env) + ", outs " + f.outs(env.myFullHand))
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