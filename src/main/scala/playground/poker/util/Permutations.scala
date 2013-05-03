package playground.poker.util

import playground.poker.cards.Card
import scala.collection.mutable.ListBuffer

/**
 * For-comprehension-friendly generator of cards permutations
 */
class Permutations(cards: List[Card], k: Int) {
  @inline
  def foreach[B](f: List[Card] => B) {
    Permutations.foreach(cards.toArray, k, 0, cards.size) { f(_) }
  }

  def map[B](f: List[Card] => B) = {
    val b = ListBuffer[B]()
    b.sizeHint(cards.size)
    for (x <- this) b += f(x)
    b.result
  }
}

object Permutations {
  def apply(cards: List[Card], k: Int): Permutations = new Permutations(cards, k)

  /**
   * This function is supposed to be as simple and as fast as possible - we need
   * every bit of raw performance while iterating through all combinations
   */
  def foreach(cards: Array[Card], cardsToDraw: Int, fromIndex: Int, toIndex: Int)(proc: List[Card] => Unit): Unit = {
    for (i <- fromIndex until toIndex) {
      val li = List(cards(i))
      if (cardsToDraw == 1) proc(li)
      else {
        for (j <- i + 1 until cards.length) {
          val lj = cards(j) :: li
          if (cardsToDraw == 2) proc(lj)
          else {
            for (k <- j + 1 until cards.length) {
              val lk = cards(k) :: lj
              if (cardsToDraw == 3) proc(lk)
              else {
                for (l <- k + 1 until cards.length) {
                  val ll = cards(l) :: lk
                  if (cardsToDraw == 4) proc(ll)
                  else {
                    for (m <- l + 1 until cards.length) {
                      val lm = cards(m) :: ll
                      if (cardsToDraw == 5) proc(lm)
                      else {
                        for (n <- m + 1 until cards.length) {
                          val ln = cards(n) :: lm
                          if (cardsToDraw == 6) proc(ln)
                          else {
                            for (o <- n + 1 until cards.length) {
                              proc(cards(o) :: ln)
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}