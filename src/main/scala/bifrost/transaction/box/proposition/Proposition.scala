package bifrost.transaction.box.proposition

import bifrost.crypto.Secret
import bifrost.serialization.BytesSerializable

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

