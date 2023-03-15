package co.topl.consensus.models

import co.topl.models.Bytes
import co.topl.models.Epoch
import co.topl.models.Eta
import co.topl.models.RhoNonceHash
import com.google.protobuf.ByteString

case class EtaCalculationArgs(previousEta: Eta, epoch: Epoch, rhoNonceHashValues: Iterable[RhoNonceHash]) {

  def digestMessages: List[Bytes] =
    previousEta.data +:
    ByteString.copyFrom(BigInt(epoch).toByteArray) +:
    rhoNonceHashValues.map(_.sizedBytes.data).toList
}
