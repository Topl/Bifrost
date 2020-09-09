package bifrost.modifier.box.serialization

import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.box.{AssetBox, TokenBox}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object AssetBoxSerializer extends BifrostSerializer[AssetBox] {

  override def serialize(obj: AssetBox, w: Writer): Unit = {

    TokenBoxSerializer.serialize(obj, w)

    /* assetCode: String */
    w.putIntString(obj.assetCode)

    /* issuer: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.issuer, w)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): AssetBox = {
    val noncedBox: TokenBox = TokenBoxSerializer.parse(r)

    /* putIntString encode String that is shorter than 2147483647 bytes */
    val asset: String = r.getIntString()

    val issuer: PublicKey25519Proposition = PublicKey25519PropositionSerializer.parse(r)
    val data: String = r.getIntString()

    AssetBox(noncedBox.proposition, noncedBox.nonce, noncedBox.value, asset, issuer, data)
  }
}