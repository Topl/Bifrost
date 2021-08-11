package co.topl.attestation.keyManagement.derivedKeys

import co.topl.utils.SizedBytes
import co.topl.utils.SizedBytes.Types.ByteVector32
import co.topl.utils.SizedBytes.implicits._
import co.topl.utils.encode.Base16
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import scodec.bits.ByteOrdering

class ExtendedPublicKeyEd25519Spec extends AnyFlatSpec {

  "ExtendedPublicKeyEd25519.derive" should "pass test vectors" in {
    val rootPrv =
      "402b03cd9c8bed9ba9f9bd6cd9c315ce9fcc59c7c25d37c85a36096617e69d418e35cb4a3b737afd007f0688618f21a8831643c0e6c77fc33c06026d2a0fc93832596435e70647d7d98ef102a32ea40319ca8fb6c851d7346d3bd8f9d1492658"
    val expectedDerivedPublic =
      "ec3d5540764b043b21b3744d37448d3f2f8b1ff7188ebec476ac190995f6b047cb1f6e0d259c2b17cb1037d84f192fd419d3f674f6a553cbecda77f0e0bfd985"

    val rootPrvBytes = Base16.decode(rootPrv).getOrElse(throw new Error())

    val rootKey =
      ExtendedPrivateKeyEd25519(
        SizedBytes[ByteVector32].fit(rootPrvBytes.slice(0, 32), ByteOrdering.LittleEndian),
        SizedBytes[ByteVector32].fit(rootPrvBytes.slice(32, 64), ByteOrdering.LittleEndian),
        SizedBytes[ByteVector32].fit(rootPrvBytes.slice(64, 96), ByteOrdering.LittleEndian),
        Seq()
      )

    val derivedPub = rootKey.public.derive(SoftIndex(0))

    Base16.encode(derivedPub.bytes.toArray ++ derivedPub.chainCode.toArray) shouldBe expectedDerivedPublic
  }
}
