package co.topl.attestation.keyManagement.wallets

import java.io._
import java.math.BigInteger
import java.nio.ByteOrder
import co.topl.attestation.keyManagement.wallets.Bip32._
import co.topl.crypto.signatures.eddsa
import org.bouncycastle.crypto.digests.{SHA256Digest, SHA512Digest}
import scodec.bits.ByteVector

/**
 * see https://github.com/satoshilabs/slips/blob/master/slip-0010.md
 */

object Slip23 {

  case class ExtendedPrivateKey(
    secretkeybytes_left:  ByteVector32,
    secretkeybytes_right: ByteVector32,
    chaincode:            ByteVector32,
    depth:                Int,
    path:                 KeyPath,
    parent:               Long
  ) {

    def sk = new BigInteger(1, secretkeybytes_left.bytes.toArray)

    def skHex = secretkeybytes_left.toString

  }

  def sha512(bytes: Array[Byte]): Array[Byte] = {
    val digest = new SHA512Digest()
    digest.update(bytes, 0, bytes.length)
    val rsData = new Array[Byte](digest.getDigestSize)
    digest.doFinal(rsData, 0)
    rsData
  }

  def sha256(bytes: Array[Byte]): Array[Byte] = {
    val digest = new SHA256Digest()
    digest.update(bytes, 0, bytes.length)
    val rsData = new Array[Byte](digest.getDigestSize)
    digest.doFinal(rsData, 0)
    rsData
  }

  case class ExtendedPublicKey(
    publickeybytes: ByteVector,
    chaincode:      ByteVector32,
    depth:          Int,
    path:           KeyPath,
    parent:         Long
  ) {
    //require(publickeybytes.length == 33)
    require(chaincode.bytes.length == 32)

    def pk = new BigInteger(1, publickeybytes.toArray)
    def pkHex = publickeybytes.toHex

  }

  /**
   * @param seed random seed
   * @return a "master" private key
   */
  def generate(seed: ByteVector): ExtendedPrivateKey = {
    val I = hmac512(ByteVector.view("ed25519 cardano seed".getBytes("UTF-8")), seed)
    val IL = ByteVector32(I.take(32))
    val chainCode = ByteVector32(I.takeRight(32))
    val path = new KeyPath(List.empty[Long])
    val k = sha512(IL.bytes.toArray)

    k(0) = (k(0) & 0xf8.toByte).toByte
    k(31) = ((k(31) & 0x1f.toByte) | 0x40.toByte).toByte

    val kL = ByteVector32(ByteVector(k).take(32))

    val kR = ByteVector32(ByteVector(k).takeRight(32))
    ExtendedPrivateKey(kL, kR, chainCode, depth = 0, path, parent = 0L)

  }

  /**
   * @param input extended private key
   * @return the public key for this private key
   */
  def publicKey(input: ExtendedPrivateKey): ExtendedPublicKey = {

    val privkey = input.secretkeybytes_left.bytes

    val ed = new eddsa.Ed25519
    val sk = privkey
    val pk = new Array[Byte](ed.PUBLIC_KEY_SIZE)

    ed.scalarMultBaseEncoded(privkey.toArray, pk, 0)

    ExtendedPublicKey(ByteVector(pk), input.chaincode, depth = input.depth, path = input.path, parent = input.parent)
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
      val buffer = parent.secretkeybytes_left.bytes ++ parent.secretkeybytes_right.bytes.+:(0.toByte)
      hmac512(parent.chaincode.bytes, buffer ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN))
    } else {

      hmac512(
        parent.chaincode.bytes,
        publicKey(parent).publickeybytes.+:(0x02.toByte) ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN)
      )
    }

    val IL = ByteVector(I.take(28).toArray)
    val IR = ByteVector32(I.takeRight(32))
    val bi = BigInteger.valueOf(8)

    val N = BigInteger.valueOf(2).pow(256)
    val bigIntKeyLeft = new BigInteger(1, (IL.toArray))
    val bigIntKeyRight = new BigInteger(1, IR.bytes.toArray)
    val bigIntParKeyLeft = new BigInteger(1, parent.secretkeybytes_left.bytes.toArray)
    val bigIntParKeyRight = new BigInteger(1, parent.secretkeybytes_right.bytes.toArray)
    val privChildLeft = bigIntKeyLeft.multiply(bi).add(bigIntParKeyLeft)
    //println("priva bigIntKeyLeft:" + bigIntKeyLeft)
    //println("priva: bigIntParKeyLeft" + parent.secretkeybytes_left)
    //println("priva:" + bigIntKeyLeft.multiply(bi))
    //val privChildLeft = bigIntKeyLeft.multiply(bi)
    val privChildRight = bigIntKeyRight.add(bigIntParKeyRight).mod(N)
    val privChildLeftBytes32 = fixSize(ByteVector.view(privChildLeft.toByteArray.dropWhile(_ == 0.toByte)))
    val privChildRightBytes32 = fixSize(ByteVector.view(privChildRight.toByteArray.dropWhile(_ == 0.toByte)))
    //scalar_multiply8(I.toArray,28,privChildLeft)
    //scalar_add_256bits(privChildLeft,parent.secretkeybytes_left.bytes.toArray,privChildLeft)
    //scalar_add_256bits(privChildRight,parent.secretkeybytes_right.bytes.toArray,privChildRight)
    val IC = if (isHardened(index)) {
      val buffer = parent.secretkeybytes_left.bytes ++ parent.secretkeybytes_right.bytes.+:(0x01.toByte)
      hmac512(parent.chaincode.bytes, buffer ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN))
    } else {
      val pub = publicKey(parent).publickeybytes.+:(0x03.toByte)
      hmac512(parent.chaincode.bytes, pub ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN))
    }

    val ICR = ByteVector32(IC.takeRight(32))

    ExtendedPrivateKey(
      privChildLeftBytes32,
      privChildRightBytes32,
      chaincode = ICR,
      depth = parent.depth + 1,
      path = parent.path.derive(index),
      parent = fingerprint(parent)
    )
  }

  /**
   * @param parent extended public key
   * @param index  index of the child key
   * @return the derived public key at the specified index
   */
  def derivePublicKey(parent: ExtendedPublicKey, index: Long, parent_priv: ExtendedPrivateKey): ExtendedPublicKey = {
    require(!isHardened(index), "Cannot derive public keys from public hardened keys")

    val I = hmac512(
      parent.chaincode.bytes,
      parent.publickeybytes.+:(0x02.toByte) ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN)
    )
    val IL = ByteVector(I.take(28).toArray)
    val IR = ByteVector32(I.takeRight(32))
    val N = BigInteger.valueOf(2).pow(256)

    val bi = BigInteger.valueOf(8)

    val bigIntParKeyLeft = new BigInteger(1, parent_priv.secretkeybytes_left.bytes.toArray)
    val bigIntKeyLeft = new BigInteger(1, IL.toArray)
    val bigIntKeyRight = new BigInteger(1, IR.bytes.toArray)
    val ChildLeft = bigIntKeyLeft.multiply(bi)
    println("pub: bigIntParKeyLeft" + parent_priv.secretkeybytes_left)
    println("pub: bigIntKeyLeft" + bigIntKeyLeft)
    println("pub:" + ChildLeft)

    val ChildLeftBytes32 = fixSize(ByteVector.view(ChildLeft.toByteArray.dropWhile(_ == 0.toByte)))

    val ed = new eddsa.Ed25519

    //val pubParKeyP = ecSpec.getG().multiply(bigIntParPubKey)
    val pkLeft = new Array[Byte](ed.PUBLIC_KEY_SIZE)
    val s = new Array[Byte](32)

    //pruneScalar(ChildLeftBytes32.bytes.toArray, 0, s)

    val p = new ed.PointAccum
    ed.scalarMultBase(ChildLeftBytes32.bytes.toArray, p)

    val pA = new ed.PointExt
    ed.decodePointVar(parent.publickeybytes.toArray, 0, true, pA)

    //pointAddVar(true,pA, p)

    val pubChildKey = new Array[Byte](32)
    val temp = new Array[Byte](32)
    //pubChildKey(0) = 0

    ed.scalarMultBaseEncoded(ChildLeftBytes32.bytes.toArray, temp, 0)

    val pB = new ed.PointExt
    val sum = new ed.PointExt
    //decodePointVar(pubChildKey, 0, true, pB)

    ed.pointAddVar(true, pA, p)
    println("Public POINT" + p)
    ed.encodePoint(p, pubChildKey, 0)

    val IC = hmac512(
      parent.chaincode.bytes,
      parent.publickeybytes.+:(0x03.toByte) ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN)
    )

    val ICR = ByteVector32(IC.takeRight(32))

    ExtendedPublicKey(
      ByteVector(pubChildKey),
      chaincode = ICR,
      depth = parent.depth + 1,
      path = parent.path.derive(index),
      parent = fingerprint(parent)
    )
  }

}
