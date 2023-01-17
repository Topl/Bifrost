package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.Txo
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.models.{BlockBody, Transaction}
import co.topl.proto.models.TypedEvidence

/**
 * Mediator of content inserted to a data store. Based on the Mediator software design pattern.
 * For more information, refer to <a href="https://en.wikipedia.org/wiki/Mediator_pattern">Mediator pattern</a>
 * @tparam F the effect-ful context to retrieve the value in
 */
trait Mediator[F[_]] {

  /**
   * Mediate after insertion of header.
   *
   * @param block the full block data for context reasons
   * @param graph graph in which to commit transaction
   * @return Unit
   */
  def afterHeaderInserted(block: BlockData): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of body.
   *
   * @param body inserted block body
   * @param block  the full block data for context reasons
   * @param graph  graph in which to commit transaction
   * @return Unit
   */
  def afterBodyInserted(body: BlockBody, block: BlockData): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of transaction.
   *
   * @param transaction inserted transaction
   * @param block  the full block data for context reasons
   * @param graph  graph in which to commit transaction
   * @return Unit
   */
  def afterTxInserted(transaction: Transaction, block: BlockData): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of address.
   *
   * @param address inserted address
   * @param block  the full block data for context reasons
   * @param graph  graph in which to commit transaction
   * @return Unit
   */
  def afterAddressInserted(address: TypedEvidence, block: BlockData): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of transaction output.
   *
   * @param txo inserted transaction output
   * @param block  the full block data for context reasons
   * @param graph  graph in which to commit transaction
   * @return Unit
   */
  def afterTxoInserted(txo: Txo, block: BlockData): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of address state.
   *
   * @param block the full block data for context reasons
   * @param graph in which to commit transaction
   * @return Unit
   */
  def afterAddressStateInserted(block: BlockData): F[Either[Failure, Unit]]

}
