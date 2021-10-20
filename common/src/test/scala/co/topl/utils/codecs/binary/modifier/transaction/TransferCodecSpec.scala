package co.topl.utils.codecs.binary.modifier.transaction

import cats.{Eq, Show}
import co.topl.modifier.transaction.Transaction
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.transaction.TransactionSerializer
import co.topl.utils.codecs.binary.modifier.codecs.transactionCodec

class TransferCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[Transaction.TX] = Eq.fromUniversalEquals
  implicit private val show: Show[Transaction.TX] = Show.fromToString

  codecCompatabilityBehavior("transaction", transactionCodec, TransactionSerializer, transferGen)
}
