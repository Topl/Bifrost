package co.topl.stakeholder

import scala.util.Try

/**
  * AMS 2020:
  * Some utilities for checking database and tine operations during chain selection
  */

trait Operations extends Members {

  /**
    * retrieve parent block id from block
    * @param b header that points to parent
    * @return parent id
    */
  def getParentId(b:BlockHeader): SlotId = {
    (b._10,b._1)
  }

  /**
    * Retrieve a block header from database
    * @param bid slot id of header to find
    * @return block if found or None
    */
  def getBlockHeader(bid:SlotId): Option[BlockHeader] =
    Try{blocks.get(bid).get.blockHeader.get}.toOption


  /**
    * Retrieve parent block
    * @param b header that points to parent
    * @return parent block if found or None
    */
  def getParentBlockHeader(b:BlockHeader): Option[BlockHeader] =
    Try{blocks.get(b._10,b._1).get.blockHeader.get}.toOption


  /**
    * retrieve parent block id
    * @param bid header that points to parent id
    * @return parent id if found, 0 otherwise
    */
  def getParentId(bid:SlotId): Option[SlotId] = {
    getBlockHeader(bid) match {
      case Some(b:BlockHeader) => Some((b._10,b._1))
      case _ => None
    }
  }

  /**
   * retrieve Nth parent block id
   * @param bid header that points to parent id
   * @return parent id if found, 0 otherwise
   */
  def getNthParentId(bid:SlotId,n:Int): SlotId = {
    assert(n>=0)
    if (n == 0) {
      bid
    } else {
      getBlockHeader(bid) match {
        case Some(b:BlockHeader) if b._3 >= 0 => getNthParentId(getParentId(b),n-1)
        case _ => bid
      }
    }
  }

  def getNonce(id:SlotId):Option[Rho] = {
    getBlockHeader(id) match {
      case Some(header:BlockHeader) => Some(header._5)
      case _ => None
    }
  }

}
