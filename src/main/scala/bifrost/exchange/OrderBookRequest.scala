package bifrost.exchange

import serializer.BuySellOrder

abstract class OrderBookRequest

case class NewOrder(buySellOrder: BuySellOrder) extends OrderBookRequest
case class Cancel(timestamp: Long, order: NewOrder) extends OrderBookRequest
case class Amend(timestamp: Long, order:NewOrder, newPrice:Option[Double], newQty:Option[Long]) extends OrderBookRequest