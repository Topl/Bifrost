package bifrost.transaction.wallet

import bifrost.{NodeViewComponent, NodeViewModifier, PersistentNodeViewModifier}
import bifrost.transaction.Transaction
import bifrost.transaction.box.proposition.Proposition

import scala.util.Try

/**
  * Abstract interface for Vault, a storage for node-specific information
  */

trait Vault[P <: Proposition, TX <: Transaction[P],
            PMOD <: PersistentNodeViewModifier[P, TX], V <: Vault[P, TX, PMOD, V]] extends NodeViewComponent {
  self: V =>

  type VersionTag = NodeViewModifier.ModifierId

  def scanOffchain(tx: TX): V

  def scanOffchain(txs: Seq[TX]): V

  def scanPersistent(modifier: PMOD): V

  def scanPersistent(modifiers: Seq[PMOD]): V = modifiers.foldLeft(this) { case (v, mod) =>
    v.scanPersistent(mod)
  }

  def rollback(to: VersionTag): Try[V]
}