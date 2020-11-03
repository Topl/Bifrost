package co.topl.nodeView.state.box

import co.topl.attestation.EvidenceProducer.syntax._
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.attestation.{Address, BoxUnlocker, Evidence, EvidenceProducer}

abstract class TokenBox ( override val evidence     : Evidence,
                          override val nonce        : Box.Nonce,
                          override val value        : TokenBox.Value,
                          override val boxTypePrefix: Box.BoxType
                        ) extends Box[TokenBox.Value](evidence, nonce, value, boxTypePrefix)


object TokenBox {
  type Value = Long

  /**
   * Generate a series of unlockers for a transactions that is used to validate the transaction
   *
   * @param from a sequence of Address -> Nonce from which the box id's for the transaction can be generated
   * @param proof a map of propositions matching the evidence contained in the given addresses, as well as proof satisfying the proposition
   * @return a set of box unlockers that
   */
  def generateUnlockers[P <: Proposition: EvidenceProducer] (from: Seq[(Address, Box.Nonce)],
                                                             proof: Map[P, Proof[P]],
                                                            ): Traversable[BoxUnlocker[P]] = {
    val evidence = proof.keys.map { prop =>
      prop.generateEvidence -> prop
    }

    from.map {
      case (addr, nonce) =>
        val boxId = BoxId.idFromEviNonce(addr.evidence, nonce)
        val boxKey = evidence.collectFirst[Proof[P]]{
          case (ev, prop) if ev == addr.evidence => proof(prop)
        }.get
        new BoxUnlocker(boxId, boxKey)
    }
  }

  def generateUnlockers[P <: Proposition] (boxIds: Seq[BoxId], proof: Proof[P]): Traversable[BoxUnlocker[P]] =
    boxIds.map(new BoxUnlocker(_, proof))

}