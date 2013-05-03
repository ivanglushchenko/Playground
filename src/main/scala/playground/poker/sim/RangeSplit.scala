package playground.poker.sim

object RangeSplit extends Enumeration {
  type RangeSplit = Value
  val One, Two, Four, Eight = Value

  def toRanges(s: RangeSplit, max: Int) = s match {
    case One => List((0, max))
    case Two => List((0, 6), (6, max))
    case Four => List((0, 3), (3, 7), (7, 12), (12, max))
    case Eight => List((0, 1), (1, 2), (2, 4), (4, 6), (6, 8), (8, 11), (11, 16), (16, max))
  }
}