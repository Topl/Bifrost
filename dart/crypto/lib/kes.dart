import 'package:bifrost_crypto/ed25519.dart';
import 'package:bifrost_crypto/impl/kes_helper.dart';
import 'package:cryptography/cryptography.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';

class KesSum {
  const KesSum();
  Future<KeyPairKesSum> createKeyPair(
      List<int> seed, int height, Int64 offset) async {
    final tree = await generateSecretKey(seed, height);
    final vk = await generateVerificationKey(tree);
    return KeyPairKesSum(
        sk: SecretKeyKesSum(tree: tree, offset: offset), vk: vk);
  }

  Future<SignatureKesSum> sign(SecretKeyKesSum sk, List<int> message) async {
    Future<SignatureKesSum> loop(
        KesBinaryTree keyTree, List<List<int>> W) async {
      if (keyTree is KesMerkleNode) {
        if (keyTree.left is KesEmpty)
          return loop(keyTree.right, [List.of(keyTree.witnessLeft)]..addAll(W));
        else
          return loop(keyTree.left, [List.of(keyTree.witnessRight)]..addAll(W));
      } else if (keyTree is KesSigningLeaf) {
        return SignatureKesSum(
            verificationKey: keyTree.vk,
            signature: await ed25519.sign(message, keyTree.sk),
            witness: W);
      } else {
        return SignatureKesSum(
            verificationKey: List.filled(32, 0x00, growable: false),
            signature: List.filled(64, 0x00, growable: false),
            witness: [[]]);
      }
    }

    return loop(sk.tree, []);
  }

  Future<bool> verify(SignatureKesSum signature, List<int> message,
      VerificationKeyKesSum vk) async {
    leftGoing(int level) => ((vk.step / kesHelper.exp(level)) % 2) == 0;
    emptyWitness() async =>
        vk.value == await kesHelper.hash(signature.verificationKey);
    singleWitness(List<int> witness) async {
      if (leftGoing(0))
        return vk.value == await kesHelper.hash(signature.verificationKey);
      else
        return vk.value ==
            await kesHelper.hash(<int>[]
              ..addAll(witness)
              ..addAll(signature.verificationKey));
    }

    multiWitness(List<List<int>> witnessList, List<int> witnessLeft,
        List<int> witnessRight, int index) async {
      if (witnessList.isEmpty)
        return vk.value ==
            await kesHelper.hash(<int>[]
              ..addAll(witnessLeft)
              ..addAll(witnessRight));
      else if (leftGoing(index))
        return multiWitness(
            witnessList.sublist(1),
            await kesHelper.hash(<int>[]
              ..addAll(witnessLeft)
              ..addAll(witnessRight)),
            witnessList.first,
            index + 1);
      else
        return multiWitness(
            witnessList.sublist(1),
            witnessList.first,
            await kesHelper.hash(<int>[]
              ..addAll(witnessLeft)
              ..addAll(witnessRight)),
            index + 1);
    }

    verifyMerkle(List<List<int>> W) async {
      if (W.isEmpty)
        return emptyWitness();
      else if (W.length == 1)
        return singleWitness(W.first);
      else if (leftGoing(0))
        return multiWitness(W.sublist(1),
            await kesHelper.hash(signature.verificationKey), W.first, 1);
      else
        return multiWitness(W.sublist(1), W.first,
            await kesHelper.hash(signature.verificationKey), 1);
    }

    return (await verifyMerkle(signature.witness)) &&
        await ed25519.verify(
            signature.signature, message, signature.verificationKey);
  }

  SecretKeyKesSum update(SecretKeyKesSum sk, int step) {
    if (step == 0) return sk;
    final totalSteps = kesHelper.exp(kesHelper.getTreeHeight(sk.tree));
    final keyTime = getCurrentStep(sk.tree);
    if (step < totalSteps && keyTime < step) {
      return _evolveKey(sk.tree, step);
    }
    throw Exception(
        "Update error - Max steps: $totalSteps, current step: $keyTime, requested increase: $step");
  }

  int getCurrentStep(KesBinaryTree tree) {
    if (tree is KesMerkleNode) {
      if (tree.left is KesEmpty && tree.right is KesSigningLeaf)
        return 1;
      else if (tree.left is KesEmpty && tree.right is KesMerkleNode)
        return getCurrentStep(tree.right) +
            kesHelper.exp(kesHelper.getTreeHeight(tree.right));
      else if (tree.right is KesEmpty) return getCurrentStep(tree.left);
    }
    return 0;
  }

  Future<KesBinaryTree> generateSecretKey(List<int> seed, int height) async {
    Future<KesBinaryTree> seedTree(List<int> seed, int height) async {
      if (height == 0) {
        final keyPair = await ed25519.generateKeyPair();
        return KesSigningLeaf(keyPair.sk, keyPair.vk);
      } else {
        final r = await kesHelper.prng(seed);
        final left = await seedTree(r.first, height - 1);
        final right = await seedTree(r.second, height - 1);
        return KesMerkleNode(r.second, await kesHelper.witness(left),
            await kesHelper.witness(right), left, right);
      }
    }

    KesBinaryTree reduceTree(KesBinaryTree fullTree) {
      if (fullTree is KesMerkleNode) {
        _eraseOldNode(fullTree.right);
        return KesMerkleNode(seed, fullTree.witnessLeft, fullTree.witnessRight,
            reduceTree(fullTree.left), KesEmpty());
      } else {
        return fullTree;
      }
    }

    final out = reduceTree(await seedTree(seed, height));
    _overwriteBytes(seed);
    return out;
  }

  Future<VerificationKeyKesSum> generateVerificationKey(
      KesBinaryTree tree) async {
    if (tree is KesMerkleNode) {
      return VerificationKeyKesSum(
          value: await kesHelper.witness(tree), step: getCurrentStep(tree));
    } else if (tree is KesSigningLeaf) {
      return VerificationKeyKesSum(
          value: await kesHelper.witness(tree), step: 0);
    } else {
      return VerificationKeyKesSum(
          value: List.filled(32, 0x00, growable: false), step: 0);
    }
  }

  _eraseOldNode(KesBinaryTree node) {
    if (node is KesMerkleNode) {
      _overwriteBytes(node.seed);
      _overwriteBytes(node.witnessLeft);
      _overwriteBytes(node.witnessRight);
      _eraseOldNode(node.left);
      _eraseOldNode(node.right);
    } else if (node is KesSigningLeaf) {
      _overwriteBytes(node.sk);
      _overwriteBytes(node.vk);
    }
  }

  _evolveKey(KesBinaryTree input, int step) async {
    final halfTotalSteps = kesHelper.exp(kesHelper.getTreeHeight(input) - 1);
    shiftStep(int step) => step % halfTotalSteps;
    if (step >= halfTotalSteps) {
      if (input is KesMerkleNode) {
        if (input.left is KesSigningLeaf && input.right is KesEmpty) {
          final keyPair = await ed25519.generateKeyPair();
          final newNode = KesMerkleNode(
              List.filled(input.seed.length, 0x00),
              input.witnessLeft,
              input.witnessRight,
              KesEmpty(),
              KesSigningLeaf(keyPair.sk, keyPair.vk));
          _eraseOldNode(input.left);
          _overwriteBytes(input.seed);
          return newNode;
        }
        if (input.left is KesMerkleNode && input.right is KesEmpty) {
          final newNode = KesMerkleNode(
            List.filled(input.seed.length, 0x00),
            input.witnessLeft,
            input.witnessRight,
            KesEmpty(),
            await _evolveKey(
                await generateSecretKey(
                    input.seed, kesHelper.getTreeHeight(input) - 1),
                shiftStep(step)),
          );
          _eraseOldNode(input.left);
          _overwriteBytes(input.seed);
          return newNode;
        }
      } else if (input is KesSigningLeaf)
        return input;
      else
        return KesEmpty();
    } else {
      if (input is KesMerkleNode && input.right is KesEmpty) {
        return KesMerkleNode(input.seed, input.witnessLeft, input.witnessRight,
            _evolveKey(input.left, shiftStep(step)), KesEmpty());
      }
      if (input is KesMerkleNode && input.left is KesEmpty) {
        return KesMerkleNode(input.seed, input.witnessLeft, input.witnessRight,
            KesEmpty(), _evolveKey(input.right, shiftStep(step)));
      } else if (input is KesSigningLeaf) {
        return input;
      }
      return KesEmpty();
    }
  }

  _overwriteBytes(List<int> bytes) {
    for (int i = 0; i < bytes.length; i++) {
      bytes[i] = SecureRandom.fast.nextInt(256);
    }
  }
}

const kesSum = KesSum();

abstract class KesBinaryTree {}

class KesMerkleNode extends KesBinaryTree {
  final List<int> seed;
  final List<int> witnessLeft;
  final List<int> witnessRight;
  final KesBinaryTree left;
  final KesBinaryTree right;

  KesMerkleNode(
      this.seed, this.witnessLeft, this.witnessRight, this.left, this.right);
}

class KesSigningLeaf extends KesBinaryTree {
  final List<int> sk;
  final List<int> vk;

  KesSigningLeaf(this.sk, this.vk);
}

class KesEmpty extends KesBinaryTree {}

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
