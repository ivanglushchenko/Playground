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
  def foreach(cards: Array[Card], k: Int, fromIndex: Int, toIndex: Int)(proc: List[Card] => Unit): Unit = k match {
    case 7 =>
      for {
        i <- fromIndex until toIndex
        j <- i + 1 until cards.length - 5
        k <- j + 1 until cards.length - 4
        l <- k + 1 until cards.length - 3
        m <- l + 1 until cards.length - 2
        n <- m + 1 until cards.length - 1
        o <- n + 1 until cards.length
      } proc(List(cards(i), cards(j), cards(k), cards(l), cards(m), cards(n), cards(o)))
    case 6 =>
      for {
        i <- fromIndex until toIndex
        j <- i + 1 until cards.length - 4
        k <- j + 1 until cards.length - 3
        l <- k + 1 until cards.length - 2
        m <- l + 1 until cards.length - 1
        n <- m + 1 until cards.length
      } proc(List(cards(i), cards(j), cards(k), cards(l), cards(m), cards(n)))
    case 5 =>
      for {
        i <- fromIndex until toIndex
        j <- i + 1 until cards.length - 3
        k <- j + 1 until cards.length - 2
        l <- k + 1 until cards.length - 1
        m <- l + 1 until cards.length
      } proc(List(cards(i), cards(j), cards(k), cards(l), cards(m)))
    case 4 =>
      for {
        i <- fromIndex until toIndex
        j <- i + 1 until cards.length - 2
        k <- j + 1 until cards.length - 1
        l <- k + 1 until cards.length
      } proc(List(cards(i), cards(j), cards(k), cards(l)))
    case 3 =>
      for {
        i <- fromIndex until toIndex
        j <- i + 1 until cards.length - 1
        k <- j + 1 until cards.length
      } proc(List(cards(i), cards(j), cards(k)))
    case 2 =>
      for {
        i <- fromIndex until toIndex
        j <- i + 1 until cards.length
      } proc(List(cards(i), cards(j)))
    case 1 =>
      for {
        i <- fromIndex until toIndex
      } proc(List(cards(i)))
    case _ =>
  }
}