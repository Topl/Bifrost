package bifrost.transaction.box.proposition

import bifrost.serialization.BytesSerializable
import bifrost.transaction.state.Secret

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

