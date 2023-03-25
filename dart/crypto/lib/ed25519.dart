import 'package:cryptography/cryptography.dart' as c;

class Ed25519 {
  static final _algorithm = c.Ed25519();

  Future<KeyPair> _convertAlgKeypair(c.SimpleKeyPair algKeypair) async {
    final sk = await algKeypair.extractPrivateKeyBytes();
    final vk = await algKeypair.extractPublicKey();
    return KeyPair(sk, vk.bytes);
  }

  Future<KeyPair> generateKeyPair() async {
    return _convertAlgKeypair(await _algorithm.newKeyPair());
  }

  Future<List<int>> sign(List<int> message, KeyPair keyPair) async {
    final algKeyPair = c.SimpleKeyPairData(
      keyPair.sk,
      publicKey: c.SimplePublicKey(keyPair.vk, type: c.KeyPairType.ed25519),
      type: c.KeyPairType.ed25519,
    );

    final algSignature = await _algorithm.sign(message, keyPair: algKeyPair);

    return algSignature.bytes;
  }

  Future<bool> verify(
      List<int> signature, List<int> message, List<int> vk) async {
    return await _algorithm.verify(
      message,
      signature: c.Signature(
        signature,
        publicKey: c.SimplePublicKey(vk, type: c.KeyPairType.ed25519),
      ),
    );
  }
}

final ed25519 = Ed25519();

class KeyPair {
  final List<int> sk;
  final List<int> vk;

  KeyPair(this.sk, this.vk);
}
