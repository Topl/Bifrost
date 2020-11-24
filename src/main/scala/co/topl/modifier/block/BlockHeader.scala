package co.topl.modifier.block

import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.modifier.NodeViewModifier
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block.{BlockId, Timestamp, Version}
import co.topl.nodeView.state.box.ArbitBox
import scorex.crypto.hash.Digest32
import supertagged.@@

case class BlockHeader( id          : BlockId,
                        parentId    : BlockId,
                        timestamp   : Timestamp,
                        forgerBox   : ArbitBox,
                        publicKey   : PublicKeyPropositionCurve25519,
                        signature   : SignatureCurve25519,
                        txRoot      : Digest32,
                        bloomFilter : BloomFilter,
                        version     : Version
                      ) extends PersistentNodeViewModifier {

  override lazy val modifierTypeId: ModifierTypeId = BlockHeader.modifierTypeId

}

object BlockHeader {
  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = ModifierTypeId @@ (5: Byte)
}
