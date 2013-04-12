package playground.poker

object Repl {
  def read() = Console.readLine("what's your hand? >> ")

  def eval(input: String): Option[String] = ReplInput.parseCmd(input) match {
    case Some(ExitCommand) => None
    case Some(HandCommand(hand)) => {
      Some("hand is: " + hand + ", best combo is: " + hand.combinations.head)
    }
    case _ => Some("do it again, please")
  }

  def print(s: String) = println(s)

  def loop() {
    eval(read()) match {
      case Some(s) => {
        print(s)
        loop()
      }
      case _ => print("Thanks for all the fish")
    }
  }
}
