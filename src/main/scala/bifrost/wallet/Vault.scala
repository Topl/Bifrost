package bifrost.wallet

import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.transaction.bifrostTransaction.GenericTransaction
import bifrost.nodeView.{NodeViewComponent, NodeViewModifier, PersistentNodeViewModifier}

import scala.util.Try

/**
  * Abstract interface for Vault, a storage for node-specific information
  */

trait Vault[P <: Proposition, TX <: GenericTransaction[P],
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
