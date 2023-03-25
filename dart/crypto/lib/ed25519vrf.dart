import 'package:cryptography/cryptography.dart';

class Ed25519VRF {
  const Ed25519VRF();
  Ed25519VRFKeyPair generateKeyPair() {
    final random = SecureRandom.safe;
    final sk = List.generate(32, (index) => random.nextInt(256));
    final vk = getVerificationKey(sk);
    return Ed25519VRFKeyPair(sk: sk, vk: vk);
  }

  List<int> getVerificationKey(List<int> secretKey) {
    throw UnimplementedError(); // TODO
  }

  bool verify(List<int> signature, List<int> message, List<int> vk) {
    throw UnimplementedError(); // TODO
  }

  List<int> sign(List<int> sk, List<int> message) {
    throw UnimplementedError(); // TODO
  }

  List<int> proofToHash(List<int> signature) {
    throw UnimplementedError(); // TODO
  }
}

const ed25519Vrf = Ed25519VRF();

class Ed25519VRFKeyPair {
  final List<int> sk;
  final List<int> vk;

  Ed25519VRFKeyPair({required this.sk, required this.vk});
}
