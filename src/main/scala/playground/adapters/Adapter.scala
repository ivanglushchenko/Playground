package playground.adapters

abstract class Adapter {
  def getResults(query: String): List[Any]
}
