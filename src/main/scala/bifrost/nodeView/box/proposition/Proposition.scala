package bifrost.nodeView.box.proposition

import bifrost.crypto.Secret
import bifrost.utils.serialization.BytesSerializable

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

