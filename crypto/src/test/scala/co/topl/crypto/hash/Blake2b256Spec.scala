package co.topl.crypto.hash

class Blake2b256Spec extends HashSpec {

  hashCheckString[Sha, Digest32](
    List(
      "test"  -> "928b20366943e2afd11ebc0eae2e53a93bf177a4fcf35bcc64d503704e65e202",
      "topl"  -> "c39310192260edc08a5fde86b81068055ea63571dbcfdcb40c533fba2d1e6d9e",
      "scala" -> "c7170ce5f424098943d56d421b5bb731cf28701c79158fc6c168968ba004c0d0",
      ""      -> "a075abe9a4c0ca81c29421dd37208ac1160ecd207541a8ec89c75a0071ae2529"
    )
  )

  hashCheckString[Sha, Digest64](
    List(
    )
  )
}
