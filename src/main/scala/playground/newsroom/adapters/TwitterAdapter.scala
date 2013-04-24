package playground.newsroom.adapters

import twitter4j._

class TwitterAdapter extends Adapter {
  def getResults(query: String): List[Any] = {
    val twitter = new TwitterFactory().getInstance
    val query = new Query("GOOG")
    val results = twitter.search(query).getTweets.toArray.toList
    for { tweet <- results } println(tweet)
    results
  }
}
