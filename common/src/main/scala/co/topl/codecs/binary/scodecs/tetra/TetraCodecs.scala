package co.topl.codecs.binary.scodecs.tetra

import cats.implicits._
import co.topl.models.Transaction
import co.topl.modifier.ops.implicits._
import scodec.{Attempt, Codec, Err}
import co.topl.codecs.binary.scodecs.modifier.transaction._

trait TetraCodecs {

  implicit val tetraTransactionCodec: Codec[Transaction] =
    transactionCodec
      .econtramap[Transaction](transaction =>
        Attempt.fromEither(transaction.toDionTx.leftMap(failure => Err(failure.toString)))
      )
      .emap(tx => Attempt.fromEither(tx.toTetraTx.leftMap(failure => Err(failure.toString))))
      .fuse

}
