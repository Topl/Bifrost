package co.topl.utils.codecs.binary.legacy.modifier.box

import co.topl.modifier.box.AssetValue
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.utils.codecs.binary.scodecs.valuetypes.Constants.stringCharacterSet

object AssetValueSerializer extends BifrostSerializer[AssetValue] {

  override def serialize(obj: AssetValue, w: Writer): Unit = {
    w.putInt128(obj.quantity)
    AssetCodeSerializer.serialize(obj.assetCode, w)
    SecurityRootSerializer.serialize(obj.securityRoot, w)
    w.putOption(obj.metadata) { (writer, metadata) =>
      writer.putByteString(new String(metadata.value, stringCharacterSet))
    }
  }

  override def parse(r: Reader): AssetValue = {
    val quantity = r.getInt128()
    val assetCode = AssetCodeSerializer.parse(r)
    val securityRoot = SecurityRootSerializer.parse(r)
    val metadata: Option[Latin1Data] = r.getOption {
      Latin1Data.unsafe(r.getByteString())
    }

    AssetValue(quantity, assetCode, securityRoot, metadata)
  }
}
