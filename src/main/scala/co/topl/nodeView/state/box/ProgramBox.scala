package co.topl.nodeView.state.box

import co.topl.attestation.proof.SignatureCurve25519
import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
import co.topl.attestation.{BoxUnlocker, Evidence}

abstract class ProgramBox (override val evidence     : Evidence,
                           override val value        : ProgramId,
                           override val nonce        : Box.Nonce,
                           override val boxTypePrefix: Box.BoxType
                          ) extends Box[ProgramId](evidence, value, nonce, boxTypePrefix)
