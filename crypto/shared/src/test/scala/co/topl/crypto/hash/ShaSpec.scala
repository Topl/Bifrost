package co.topl.crypto.hash

import co.topl.crypto.hash.digest.{Digest32, Digest64}
import co.topl.crypto.hash.implicits._

class ShaSpec extends HashSpec {

  testHash[Sha, Digest32](
    "Sha256",
    List(
      "test"  -> "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08",
      "topl"  -> "8a475240db931554ab61d117d791711f38728fa45c7d39d18d49b7ceaf983ae8",
      "scala" -> "a1c176cc5c0cbe6de947836adf270a2d8d53164b5dc617b4816a2d759fd056b6",
      ""      -> "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
  )

  testHash[Sha, Digest64](
    "Sha512",
    List(
      "test" -> "ee26b0dd4af7e749aa1a8ee3c10ae9923f618980772e473f8819a5d4940e0db27ac185f8a0e1d5f84f88bc887fd67b143732c304cc5fa9ad8e6f57f50028a8ff",
      "topl" -> "867aef1cdfc88d30a3ab2f63a20157bc7dc5b182fc625dbf29458c5d17268fd968ef39ce44ca00be57fc255de7bc3c15182a8290fcc673bcd95a5f306468e566",
      "scala" -> "9cd0cba7f2884c11e229ea75cea65eefd8721f986242a405343f3c594520c4a25862b907b6dfe60dfaf430300e54530a6f20080b17aad439a8f9a168d36071f5",
      "" -> "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
    )
  )

}
