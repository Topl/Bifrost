package co.topl.genusLibrary.interpreter

import cats.effect._
import cats.implicits._
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.genus.services.{BlockData, Txo, TxoState}
import co.topl.genusLibrary.algebras.BlockInserterAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.genusLibrary.orientDb.instances.SchemaLockAddress
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genusLibrary.orientDb.instances.SchemaBlockHeader.Field
import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances._
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import org.typelevel.log4cats.Logger
import scala.util.Try

object GraphBlockInserter {

  def make[F[_]: OrientThread: Logger](graph: OrientGraph): Resource[F, BlockInserterAlgebra[F]] =
    Resource.pure(
      new BlockInserterAlgebra[F] {

        override def insert(block: BlockData): F[Either[GE, Unit]] =
          OrientThread[F].delay {
            Try {
              val headerVertex = graph.addHeader(block.header)

              graph.addCanonicalHead(headerVertex)

              val bodyVertex = graph.addBody(block.body)
              bodyVertex.setProperty(blockBodySchema.links.head.propertyName, headerVertex.getId)

              // Relationships between Header <-> TxIOs
              block.transactions.foreach { ioTx =>
                val ioTxVertex = graph.addIoTx(ioTx)
                ioTxVertex.setProperty(ioTransactionSchema.links.head.propertyName, headerVertex.getId)
                graph.addEdge(s"class:${blockHeaderTxIOEdge.name}", headerVertex, ioTxVertex, blockHeaderTxIOEdge.label)

                // TODO question
                //  ioTx.outputs.inputs.address.TransactionOutputAddress foreach, fetch the txo, and change the status to SPENT?
                // if, yes the Txo model should have the transactionId.
                // when a txo is spent, is fully spent or partially spent?

                // Relationships between TxIOs <-> LockAddress
                ioTx.outputs.zipWithIndex.foreach { case (utxo, index) =>
                  // before adding a new address, check if was not there included by a previous transaction
                  val lockAddressVertex = {
                    val addressIterator =
                      graph.getVertices(SchemaLockAddress.Field.AddressId, utxo.address.id.value.toByteArray).iterator()

                    if (addressIterator.hasNext) addressIterator.next()
                    else graph.addAddress(utxo.address)

                  }
                  graph.addEdge(s"class:${addressTxIOEdge.name}", lockAddressVertex, ioTxVertex, addressTxIOEdge.label)

                  val txoVertex = graph.addTxo(
                    Txo(
                      utxo,
                      TxoState.UNSPENT,
                      TransactionOutputAddress(utxo.address.network, utxo.address.ledger, index, ioTx.id)
                    )
                  )
                  graph.addEdge(s"class:${addressTxoEdge.name}", lockAddressVertex, txoVertex, addressTxoEdge.label)
                }

              }

              // Relationship between Header <-> ParentHeader if Not Genesis block
              if (block.header.height != 1) {
                val headerInVertex = graph
                  .getVertices(Field.BlockId, block.header.parentHeaderId.value.toByteArray)
                  .iterator()
                  .next()

                graph.addEdge(s"class:${blockHeaderEdge.name}", headerVertex, headerInVertex, blockHeaderEdge.label)
              }

              graph.commit()
            }.toEither
              .leftMap { th =>
                graph.rollback()
                GEs.InternalMessage(th.getMessage): GE
              }
          }

      }
    )

}
