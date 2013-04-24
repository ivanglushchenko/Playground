package playground.poker.repl

import java.util.Calendar
import java.text.SimpleDateFormat
import playground.poker.{Environment}

object Repl {
  def apply() {
    loop(Environment())
  }

  def read() = {
    val today = Calendar.getInstance().getTime()
    val prompt = new SimpleDateFormat("HH").format(today) + ":" + new SimpleDateFormat("mm").format(today) + " >> "
    ReplInput.parseCmd(Console readLine prompt)
  }

  def loop(env: Environment) {
    read() match {
      case Some(command) => command(env) match {
        case Some(newEnv) => loop(newEnv)
        case None => ()
      }
      case None => {
        println("! command is not recognized")
        loop(env)
      }
    }
  }
}
