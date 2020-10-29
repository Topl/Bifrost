package co.topl.nodeView.state.box.serialization

import co.topl.attestation.proposition.{PublicKeyCurve25519Proposition, PublicKeyCurve25519PropositionSerializer}
import co.topl.nodeView.state.box.AssetBox
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object AssetBoxSerializer extends BifrostSerializer[AssetBox] {

  override def serialize(obj: AssetBox, w: Writer): Unit = {

    TokenBoxSerializer.serialize(obj, w)

    /* assetCode: String */
    w.putIntString(obj.assetCode)

    /* issuer: PublicKey25519Proposition */
    PublicKeyCurve25519PropositionSerializer.serialize(obj.issuer, w)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): AssetBox = {
    val (proposition, nonce, value) = TokenBoxSerializer.parse(r)

    /* putIntString encode String that is shorter than 2147483647 bytes */
    val asset: String = r.getIntString()

    val issuer: PublicKeyCurve25519Proposition = PublicKeyCurve25519PropositionSerializer.parse(r)
    val data: String = r.getIntString()

    AssetBox(proposition, nonce, value, asset, issuer, data)
  }
}