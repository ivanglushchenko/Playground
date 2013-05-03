package playground.poker.sim

/**
 * Signals that a range of hands has been successfully processed
 */
case class MsgHandRangeProcCompleted(combinations: Array[Int])
