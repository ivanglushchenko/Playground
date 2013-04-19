package playground.poker

import scala.actors.Actor
import scala.actors.Actor._

class Dispatcher(myCards: List[Card], remainingCards: Array[Card], val numOfWorkers: Int) {
  val s = new Simulator()

}

class Simulator extends Actor {
  def act() {

  }
}
