package co.topl.codecs.binary.scodecs.modifier.transaction

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.transaction.TransactionSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class TransferCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("transaction", transactionCodec, TransactionSerializer, transferGen)
}
