package co.topl.consensus.models

import co.topl.models.{Bytes, Epoch, Eta, RhoNonceHash}

case class EtaCalculationArgs(previousEta: Eta, epoch: Epoch, rhoNonceHashValues: Iterable[RhoNonceHash]) {

  def digestMessages: List[Bytes] =
    previousEta.data +:
    Bytes(BigInt(epoch).toByteArray) +:
    rhoNonceHashValues.map(_.sizedBytes.data).toList
}
