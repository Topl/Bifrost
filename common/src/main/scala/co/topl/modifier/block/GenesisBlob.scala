package co.topl.modifier.block

import co.topl.attestation.keyManagement.PrivateKeyCurve25519

case class GenesisBlob(genesis: Block, keys: Seq[PrivateKeyCurve25519])
