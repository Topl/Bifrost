package co.topl.codecs.binary.scodecs

trait ScodecCodecs
    extends attestation.AttestationCodecs
    with crypto.CryptoCodecs
    with modifier.ModifierCodecs
    with valuetypes.ValuetypesCodecs
    with genesisBlob.GenesisBlobCodecs
