import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';

class KesSum {
  const KesSum();
  KeyPairKesSum createKeyPair(List<int> seed, int height, Int64 offset) {
    final tree = generateSecretKey(seed, height);
    final vk = generateVerificationKey(tree);
    return KeyPairKesSum(
        sk: SecretKeyKesSum(tree: tree, offset: offset), vk: vk);
  }

  SignatureKesSum sign(SecretKeyKesSum sk, List<int> message) {
    throw UnimplementedError();
  }

  bool verify(
      SignatureKesSum signature, List<int> message, VerificationKeyKesSum vk) {
    throw UnimplementedError();
  }

  SecretKeyKesSum update(SecretKeyKesSum sk, int steps) {
    throw UnimplementedError();
  }

  int getCurrentStep(SecretKeyKesSum sk) {
    throw UnimplementedError();
  }

  KesBinaryTree generateSecretKey(List<int> seed, int height) {
    throw UnimplementedError();
  }

  VerificationKeyKesSum generateVerificationKey(KesBinaryTree tree) {
    throw UnimplementedError();
  }
}

const kesSum = KesSum();

abstract class KesBinaryTree {}

class SecretKeyKesSum {
  final KesBinaryTree tree;
  final Int64 offset;

  SecretKeyKesSum({required this.tree, required this.offset});
}

class VerificationKeyKesSum {
  final List<int> value;
  final int step;

  VerificationKeyKesSum({required this.value, required this.step});
}

class KeyPairKesSum {
  final SecretKeyKesSum sk;
  final VerificationKeyKesSum vk;

  KeyPairKesSum({required this.sk, required this.vk});
}

class KesProduct {
  const KesProduct();
  KeyPairKesProduct createKeyPair(
      List<int> seed, TreeHeight height, Int64 offset) {
    final sk = generateSecretKey(seed, height);
    final vk = generateVerificationKey(sk);
    return KeyPairKesProduct(sk: sk, vk: vk);
  }

  SignatureKesProduct sign(SecretKeyKesProduct sk, List<int> message) {
    throw UnimplementedError(); // TODO
  }

  bool verify(SignatureKesProduct signature, List<int> message,
      VerificationKeyKesProduct vk) {
    throw UnimplementedError(); // TODO
  }

  SecretKeyKesProduct update(SecretKeyKesProduct sk, int steps) {
    throw UnimplementedError(); // TODO
  }

  int getCurrentStep(SecretKeyKesProduct sk) {
    throw UnimplementedError(); // TODO
  }

  SecretKeyKesProduct generateSecretKey(List<int> seed, TreeHeight height) {
    throw UnimplementedError(); // TODO
  }

  VerificationKeyKesProduct generateVerificationKey(SecretKeyKesProduct sk) {
    throw UnimplementedError(); // TODO
  }
}

const kesProduct = KesProduct();

class TreeHeight {
  final int x;
  final int y;

  TreeHeight(this.x, this.y);
}

class SecretKeyKesProduct {
  final KesBinaryTree superTree;
  final KesBinaryTree subTree;
  final List<int> nextSubSeed;
  final SignatureKesSum subSignature;
  final Int64 offset;

  SecretKeyKesProduct(
      {required this.superTree,
      required this.subTree,
      required this.nextSubSeed,
      required this.subSignature,
      required this.offset});
}

class KeyPairKesProduct {
  final SecretKeyKesProduct sk;
  final VerificationKeyKesProduct vk;

  KeyPairKesProduct({required this.sk, required this.vk});
}
