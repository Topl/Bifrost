package co.topl.crypto.hash

import co.topl.crypto.hash.digest.{Digest32, Digest64}
import co.topl.crypto.hash.implicits._

class Blake2bSpec extends HashSpec {

  testHash[Blake2b, Digest32](
    "Blake2b256",
    List(
      "test"  -> "928b20366943e2afd11ebc0eae2e53a93bf177a4fcf35bcc64d503704e65e202",
      "topl"  -> "c39310192260edc08a5fde86b81068055ea63571dbcfdcb40c533fba2d1e6d9e",
      "scala" -> "c7170ce5f424098943d56d421b5bb731cf28701c79158fc6c168968ba004c0d0",
      ""      -> "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
    )
  )

  testHash[Blake2b, Digest64](
    "Blake2b512",
    List(
      "test" -> "a71079d42853dea26e453004338670a53814b78137ffbed07603a41d76a483aa9bc33b582f77d30a65e6f29a896c0411f38312e1d66e0bf16386c86a89bea572",
      "topl" -> "87c15da49659c9ed4a1b594d7bd8a9e51cca576c4d68625787253474abaaec0d942d14cbe8570709b5872c66e01de9e0cc033f0875820497060554111add78be",
      "scala" -> "dc0907da1939f0fa4ff48353a43d2a3face5ced2626ee2a151909643ea3988800e3b66a262541ec23bc6f38f3cc3a3d6a6a2ffe3295b0aa7340e4b4d91c20dda",
      "" -> "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
    )
  )
}
