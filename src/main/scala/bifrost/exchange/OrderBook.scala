package bifrost.exchange

import scala.collection.mutable

class OrderBook(symbol: String) {

  case class Order(timestamp: Long,
                   tradeId: String,
                   symbol: String,
                   var token1Qty: Long,
                   var token2Qty: Long,
                   var price: Int,
                   isBuy: Boolean,
                   newOrderEvent: NewOrder)

  val bidOrdering = Ordering.by { order: Order => (order.token1Qty / order.token2Qty, order.timestamp) }
  val offerOrdering = bidOrdering.reverse

  object OrderOrdering extends Ordering[Order] {
    def compare(a:Order, b:Order): Int = {
      val firstComp = a.price compare b.price
      if(firstComp == 0) {
        val secondComp = a.timestamp compare b.timestamp
        if (secondComp == 0) {
          0
        } else {
          secondComp
        }
      } else {
        firstComp
      }
    }
  }

  //Needed for java.util.PriorityQueue
//  val bidComparator = new Comparator[Order] {
//    def compare(o1: Order, o2: Order): Int = bidOrdering.compare(o1, o2)
//  }
//  val offerComparator = new Comparator[Order] {
//    def compare(o1: Order, o2: Order): Int = offerOrdering.compare(o1, o2)
//  }

  val bidsQ = new mutable.PriorityQueue[Order]()(OrderOrdering)
  val offersQ = new mutable.PriorityQueue[Order]()(OrderOrdering.reverse)

  //scala PQ doesn't let me remove items, so must revert to Java's PQ
//  val bidsQ = new PriorityQueue[Order](5, bidComparator)
//  val offersQ = new PriorityQueue[Order](5, offerComparator)

  var bestBid: Option[Order] = None
  var bestOffer: Option[Order] = None
  var volume: Long = 0

//  var transactionObserver: (OrderBookResponse) => Unit = (OrderBookEvent => ())
//  var marketdataObserver: (MarketDataEvent) => Unit = (MarketDataEvent => ())
}