package co.topl.utils.codecs.binary.legacy.modifier.box

import co.topl.modifier.box.AssetCode
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.codecs.binary.legacy.attestation.AddressSerializer
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object AssetCodeSerializer extends BifrostSerializer[AssetCode] {

  override def serialize(obj: AssetCode, w: Writer): Unit = {
    // should be safe to assume Latin-1 encoding since AssetCode already checks this once instantiation
    val paddedShortName = obj.shortName.value.padTo(AssetCode.shortNameLimit, 0: Byte)

    w.put(obj.version)
    AddressSerializer.serialize(obj.issuer, w)
    w.putBytes(paddedShortName)
  }

  override def parse(r: Reader): AssetCode =
    new AssetCode(
      r.getByte(),
      AddressSerializer.parse(r),
      Latin1Data.fromData(r.getBytes(AssetCode.shortNameLimit).filter(_ != 0))
    )
}
