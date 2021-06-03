package co.topl.attestation.keyManagement.wallets

import java.io._
import java.lang.reflect.Constructor
import java.math.BigInteger
import java.nio.{ByteBuffer, ByteOrder}

import co.topl.attestation.keyManagement.wallets.Bip32._
import org.whispersystems.curve25519.OpportunisticCurve25519Provider
import scodec.bits._

/**
 * see https://github.com/satoshilabs/slips/blob/master/slip-0010.md
 */

object Slip10Curve25519 {

  private val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider].getDeclaredConstructors.head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }

  case class ExtendedPrivateKey(
    secretkeybytes: ByteVector32,
    chaincode:      ByteVector32,
    depth:          Int,
    path:           KeyPath,
    parent:         Long
  ) {

    def sk = new BigInteger(1, secretkeybytes.bytes.toArray)
    def skHex = secretkeybytes.toString

  }

  object ExtendedPrivateKey {

    def decode(input: String, parentPath: KeyPath = KeyPath.Root): (ExtendedPrivateKey) = {
      val bis = new ByteArrayInputStream(input.getBytes)
      val depth = uint8(bis)
      val parent = uint32(bis, ByteOrder.BIG_ENDIAN)
      val childNumber = uint32(bis, ByteOrder.BIG_ENDIAN)
      val chaincode = ByteVector32(bytes(bis, 32))
      require(bis.read() == 0)
      val secretkeybytes = ByteVector32(bytes(bis, 32))
      (ExtendedPrivateKey(secretkeybytes, chaincode, depth, parentPath.derive(childNumber), parent))
    }
  }

  case class ExtendedPublicKey(
    publickeybytes: ByteVector,
    chaincode:      ByteVector32,
    depth:          Int,
    path:           KeyPath,
    parent:         Long
  ) {
    require(publickeybytes.length == 33)
    require(chaincode.bytes.length == 32)

    def pk = new BigInteger(1, publickeybytes.toArray)
    def pkHex = publickeybytes.toHex

  }

  object ExtendedPublicKey {

    def decode(input: String, parentPath: KeyPath = KeyPath.Root): (ExtendedPublicKey) = {
      // val (prefix, bin) = Base58Check.decodeWithIntPrefix(input)
      val bis = new ByteArrayInputStream(input.getBytes)
      val depth = uint8(bis)
      val parent = uint32(bis, ByteOrder.BIG_ENDIAN)
      val childNumber = uint32(bis, ByteOrder.BIG_ENDIAN)
      val chaincode = ByteVector32(bytes(bis, 32))
      val publickeybytes = bytes(bis, 33)
      (ExtendedPublicKey(publickeybytes, chaincode, depth, parentPath.derive(childNumber), parent))
    }
  }

  /**
   * @param seed random seed
   * @return a "master" private key
   */
  def generate(seed: ByteVector): ExtendedPrivateKey = {
    //val I = hmac512(ByteVector.view("ed25519 seed".getBytes("UTF-8")), seed)
    val I = hmac512(ByteVector.view("curve25519 seed".getBytes("UTF-8")), seed)
    val IL = ByteVector32(I.take(32))
    val IR = ByteVector32(I.takeRight(32))
    val path = new KeyPath(List.empty[Long])

    val privkey = new BigInteger(1, IL.bytes.toArray)

    ExtendedPrivateKey(IL, IR, depth = 0, path, parent = 0L)

  }

  /**
   * @param input extended private key
   * @return the public key for this private key
   */
  def publicKey(input: ExtendedPrivateKey): ExtendedPublicKey = {

    val privkey = new BigInteger(1, input.secretkeybytes.bytes.toArray)

    /*val ec = new eddsa.Ed25519
    val sk = privkey.toByteArray
    val pk = new Array[Byte](ec.PUBLIC_KEY_SIZE)
    val  out = ec.generatePublicKey(sk, 0, pk, 0)
    val pubKeyInt = new BigInteger(1, pk)*/

    val pubKeyIntCurve = provider.generatePublicKey(input.secretkeybytes.bytes.toArray)
    val pubKeyByte = ByteBuffer.allocate(33)
    pubKeyByte.put((0x00).toByte)
    pubKeyByte.put(pubKeyIntCurve)
    println("Private Key: " + input.secretkeybytes.bytes.toHex)
    println("Public Key: " + ByteVector(pubKeyByte.array()))
    ExtendedPublicKey(
      ByteVector(pubKeyByte.array()),
      input.chaincode,
      depth = input.depth,
      path = input.path,
      parent = input.parent
    )

  }

  /**
   * @param input extended public key
   * @return the fingerprint for this public key
   */
  def fingerprint(input: ExtendedPublicKey): Long = uint32(
    new ByteArrayInputStream(hash160(ByteVector(input.pk.toByteArray)).take(4).reverse.toArray)
  )

  /**
   * @param input extended private key
   * @return the fingerprint for this private key (which is based on the corresponding public key)
   */
  def fingerprint(input: ExtendedPrivateKey): Long = fingerprint(publicKey(input))

  /**
   * @param parent extended private key
   * @param index  index of the child key
   * @return the derived private key at the specified index
   */
  def derivePrivateKey(parent: ExtendedPrivateKey, index: Long): ExtendedPrivateKey = {
    val I = if (isHardened(index)) {
      val buffer = parent.secretkeybytes.bytes.+:(0.toByte)
      hmac512(parent.chaincode.bytes, buffer ++ writeUInt32(index.toInt, ByteOrder.BIG_ENDIAN))
    } else {
      throw new RuntimeException("cannot generated child private key")
    }
    val IL = ByteVector32(I.take(32))
    val IR = ByteVector32(I.takeRight(32))

    val bigIntKey = new BigInteger(1, IL.bytes.toArray)

    ExtendedPrivateKey(
      ByteVector32(ByteVector(IL.bytes.toArray)),
      chaincode = IR,
      depth = parent.depth + 1,
      path = parent.path.derive(index),
      parent = fingerprint(parent)
    )
  }

  //def derivePrivateKey(parent: ExtendedPrivateKey, chain: Seq[Long]): ExtendedPrivateKey = chain.foldLeft(parent)(derivePrivateKey)

  //def derivePrivateKey(parent: ExtendedPrivateKey, keyPath: KeyPath): ExtendedPrivateKey = derivePrivateKey(parent, keyPath.path)

  //def derivePublicKey(parent: ExtendedPublicKey, chain: Seq[Long]): ExtendedPublicKey = chain.foldLeft(parent)(derivePublicKey)

  //def derivePublicKey(parent: ExtendedPublicKey, keyPath: KeyPath): ExtendedPublicKey = derivePublicKey(parent, keyPath.path)

}
