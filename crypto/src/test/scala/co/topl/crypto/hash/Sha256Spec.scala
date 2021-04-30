package co.topl.crypto.hash

class Sha256Spec extends HashSpec {

  hashCheckString[Sha, Digest32](
    List(
      "test"  -> "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08",
      "topl"  -> "8a475240db931554ab61d117d791711f38728fa45c7d39d18d49b7ceaf983ae8",
      "scala" -> "a1c176cc5c0cbe6de947836adf270a2d8d53164b5dc617b4816a2d759fd056b6",
      ""      -> "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
  )

  hashCheckString[Sha, Digest64](
    List(

    )
  )
}
