//package co.topl.wallet
//
//import co.topl.modifier.ModifierId
//import co.topl.modifier.transaction.GenericTransaction
//import co.topl.nodeView.{ NodeViewComponent, PersistentNodeViewModifier }
//
//import scala.util.Try
//
///**
//  * Abstract interface for Vault, a storage for node-specific information
//  */
//
//trait Vault[TX <: GenericTransaction[_],
//            PMOD <: PersistentNodeViewModifier, V <: Vault[TX, PMOD, V]] extends NodeViewComponent with VaultReader {
//  self: V =>
//
//  type VersionTag = ModifierId
//
//  def scanOffchain(tx: TX): V
//
//  def scanOffchain(txs: Seq[TX]): V
//
//  def scanPersistent(modifier: PMOD): V
//
//  def scanPersistent(modifiers: Seq[PMOD]): V = modifiers.foldLeft(this) { case (v, mod) =>
//    v.scanPersistent(mod)
//  }
//
//  def rollback(to: VersionTag): Try[V]
//
//  def getReader: VaultReader = this
//}
