package co.topl.components

import co.topl.primitives.Types._
import scala.math.BigInt

/**
  * AMS 2020:
  * Simple account transactions, used for research and experiments
  */

case class Transaction(sender:PublicKeyW,receiver:PublicKeyW,delta:BigInt,sid:Sid,nonce:Int,signature: Signature)