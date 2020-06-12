package bifrost.utils

import bifrost.modifier.ModifierId
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.encode.BytesEncoder
import scorex.crypto.encode.Base58

import scala.util.Try

class BifrostEncoder extends BytesEncoder {

  @inline
  override def encode(input: Array[Byte]): String = Base58.encode(input)

  @inline
  override def decode(input: String): Try[Array[Byte]] = Base58.decode(input)

  /**
    * This method might be useful and reimplemented, if encoding of ModifierId and VersionTag
    * is different form default bytes encoding, e.g. this method should be reimplemented together
    * with encode() and decode methods
    */
  @inline
  def encode(input: String): String = input

  /**
    * This method might be useful and reimplemented, if encoding of ModifierId and VersionTag
    * is different form default bytes encoding, e.g. this method should be reimplemented together
    * with encode() and decode methods
    */
  @inline
  def encodeVersion(input: VersionTag): String = Base58.encode(input.hashBytes)

  /**
    * This method might be useful and reimplemented, if encoding of ModifierId and VersionTag
    * is different form default bytes encoding, e.g. this method should be reimplemented together
    * with encode() and decode methods
    */
  @inline
  def encodeId(input: ModifierId): String = Base58.encode(input.hashBytes)

}

object BifrostEncoder {
  val default: BifrostEncoder = new BifrostEncoder()
}