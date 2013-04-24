package playground.newsroom.adapters

abstract class Adapter {
  def getResults(query: String): List[Any]
}
