package co.topl.codecs.json

trait JsonCodecs
    extends crypto.CryptoJsonCodecs
    with valuetypes.ValueTypesJsonCodecs
    with attestation.AttestationJsonCodecs
    with attestation.keyManagement.KeyManagementJsonCodecs
    with modifier.ModifierJsonCodecs
    with genesisBlob.GenesisBlobJsonCodecs
