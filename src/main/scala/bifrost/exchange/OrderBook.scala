package bifrost.exchange

import scala.collection.mutable

class OrderBook(symbol: String) {

  case class Order(timestamp: Long,
                   tradeId: String,
                   symbol: String,
                   var token1Qty: Long,
                   var token2Qty: Long,
                   isBuy: Boolean) {
    val price = (BigDecimal(token1Qty) / BigDecimal(token2Qty)).setScale(20, BigDecimal.RoundingMode.HALF_UP)
  }

  val bidOrdering = Ordering.by { order: Order => order.price }
  val offerOrdering = bidOrdering.reverse

  val bidsQ = new mutable.PriorityQueue[Order]()(bidOrdering)
  val offersQ = new mutable.PriorityQueue[Order]()(offerOrdering)

  var bestBid: Option[Order] = None
  var bestOffer: Option[Order] = None
  var volume: Long = 0

  private def processNewOrder(odo: Order) = {
    val orderQ = if (odo.isBuy) bidsQ else offersQ
    val oppositeQ = if (odo.isBuy) offersQ else bidsQ

    if (oppositeQ.isEmpty || !isLimitOrderExecutable(odo, oppositeQ.head)) {
      // Add to queue, when no price match is found
      orderQ.enqueue(odo)
      // updateBBO()
    } else {
      // price is matched, try to match orders
      matchOrder(odo, oppositeQ)
    }
  }

  private def isLimitOrderExecutable(order: Order, oppositeOrder: Order): Boolean = {
    if (order.isBuy) order.price >= oppositeOrder.price
    else order.price <= oppositeOrder.price
  }

  private def matchOrder(order: Order, oppositeQ: mutable.PriorityQueue[Order]): Unit = {
    val oppositeOrder = oppositeQ.head

    if (order.token1Qty < oppositeOrder.token1Qty) {
      // Opposite side has more tokens, update the order
      oppositeOrder.token1Qty = oppositeOrder.token1Qty - order.token1Qty
      oppositeOrder.token2Qty = oppositeOrder.token2Qty - order.token2Qty

      this.volume += order.token1Qty

//      this.transactionObserver(Filled(currentTime, order.price.get, order.qty, Array(order.newOrderEvent, oppositeOrder.newOrderEvent)))
//      this.marketdataObserver(LastSalePrice(currentTime, order.symbol, order.price.get, order.qty, volume))
//      updateBBO()
    } else if (order.token1Qty > oppositeOrder.token1Qty) {
      // Opposite side has less tokens, delete the first order
      oppositeQ.dequeue()
      val reducedQty1 = order.token1Qty - oppositeOrder.token1Qty
      val reducedQty2 = order.token2Qty - oppositeOrder.token2Qty
      order.token1Qty = reducedQty1
      order.token2Qty = reducedQty2

      this.volume += order.token1Qty

//      this.transactionObserver(Filled(currentTime, order.price.get, order.qty, Array(order.newOrderEvent, oppositeOrder.newOrderEvent)))
//      this.marketdataObserver(LastSalePrice(currentTime, order.symbol, order.price.get, order.qty, volume))
//      updateBBO()

      processNewOrder(order)
    } else {
      oppositeQ.dequeue()

      this.volume += order.token1Qty

//      this.transactionObserver(Filled(currentTime, order.price.get, order.qty, Array(order.newOrderEvent, oppositeOrder.newOrderEvent)))
//      this.marketdataObserver(LastSalePrice(currentTime, order.symbol, order.price.get, order.qty, volume))
//      updateBBO()
    }
  }

//  var transactionObserver: (OrderBookResponse) => Unit = (OrderBookEvent => ())
//  var marketdataObserver: (MarketDataEvent) => Unit = (MarketDataEvent => ())

}