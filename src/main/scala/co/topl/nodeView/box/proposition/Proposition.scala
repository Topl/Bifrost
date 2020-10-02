package co.topl.nodeView.box.proposition

import co.topl.crypto.Secret
import co.topl.utils.serialization.BytesSerializable

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

