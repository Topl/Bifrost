package co.topl.modifier.block

import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.crypto.hash.digest.Digest32
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.utils.TimeProvider

case class BlockHeader(
  id:           ModifierId,
  parentId:     ModifierId,
  timestamp:    TimeProvider.Time,
  generatorBox: ArbitBox,
  publicKey:    PublicKeyPropositionCurve25519,
  signature:    SignatureCurve25519,
  height:       Long,
  difficulty:   Long,
  txRoot:       Digest32,
  bloomFilter:  BloomFilter,
  version:      PNVMVersion
) extends PersistentNodeViewModifier {

  override lazy val modifierTypeId: ModifierTypeId = BlockHeader.modifierTypeId

}

object BlockHeader {
  val modifierTypeId: NodeViewModifier.ModifierTypeId = ModifierTypeId(4: Byte)
}
