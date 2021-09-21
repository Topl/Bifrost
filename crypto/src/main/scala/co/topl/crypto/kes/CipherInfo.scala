package co.topl.crypto.kes

case class CipherInfo(
  pubKey:     Array[Byte],
  cipherText: Array[Byte],
  mac:        Array[Byte],
  salt:       Array[Byte],
  iv:         Array[Byte]
)
