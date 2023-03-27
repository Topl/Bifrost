import 'dart:typed_data';

import 'package:bifrost_crypto/ed25519.dart';
import 'package:bifrost_crypto/impl/kes_helper.dart';
import 'package:bifrost_crypto/utils.dart';
import 'package:cryptography/cryptography.dart';
import 'package:fixnum/fixnum.dart';
import 'package:fpdart/fpdart.dart';
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

  Future<SignatureKesSum> sign(KesBinaryTree skTree, List<int> message) async {
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

    return loop(skTree, []);
  }

  Future<bool> verify(SignatureKesSum signature, List<int> message,
      VerificationKeyKesSum vk) async {
    leftGoing(int level) => ((vk.step / kesHelper.exp(level)) % 2) == 0;
    emptyWitness() async =>
        vk.value.sameElements(await kesHelper.hash(signature.verificationKey));
    singleWitness(List<int> witness) async {
      if (leftGoing(0))
        return vk.value
            .sameElements(await kesHelper.hash(signature.verificationKey));
      else
        return vk.value.sameElements(await kesHelper.hash(<int>[]
          ..addAll(witness)
          ..addAll(signature.verificationKey)));
    }

    multiWitness(List<List<int>> witnessList, List<int> witnessLeft,
        List<int> witnessRight, int index) async {
      if (witnessList.isEmpty)
        return vk.value.sameElements(await kesHelper.hash(<int>[]
          ..addAll(witnessLeft)
          ..addAll(witnessRight)));
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

  Future<KesBinaryTree> update(KesBinaryTree tree, int step) async {
    if (step == 0) return tree;
    final totalSteps = kesHelper.exp(kesHelper.getTreeHeight(tree));
    final keyTime = getCurrentStep(tree);
    if (step < totalSteps && keyTime < step) {
      return await _evolveKey(tree, step);
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

  Future<KesBinaryTree> _evolveKey(KesBinaryTree input, int step) async {
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
            await _evolveKey(input.left, shiftStep(step)), KesEmpty());
      }
      if (input is KesMerkleNode && input.left is KesEmpty) {
        return KesMerkleNode(input.seed, input.witnessLeft, input.witnessRight,
            KesEmpty(), await _evolveKey(input.right, shiftStep(step)));
      } else if (input is KesSigningLeaf) {
        return input;
      }
      return KesEmpty();
    }
    return KesEmpty();
  }
}

const kesSum = KesSum();

_overwriteBytes(List<int> bytes) {
  for (int i = 0; i < bytes.length; i++) {
    bytes[i] = SecureRandom.fast.nextInt(256);
  }
}

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
  Future<KeyPairKesProduct> generateKeyPair(
      List<int> seed, TreeHeight height, Int64 offset) async {
    final sk = await generateSecretKey(seed, height);
    final vk = await generateVerificationKey(sk);
    return KeyPairKesProduct(sk: sk, vk: vk);
  }

  Future<SignatureKesProduct> sign(
      SecretKeyKesProduct sk, List<int> message) async {
    return SignatureKesProduct(
        superSignature: sk.subSignature,
        subSignature: await kesSum.sign(sk.subTree, message),
        subRoot: (await kesSum.generateVerificationKey(sk.subTree)).value);
  }

  Future<bool> verify(SignatureKesProduct signature, List<int> message,
      VerificationKeyKesProduct vk) async {
    final totalStepsSub =
        kesHelper.exp(signature.superSignature.witness.length);
    final keyTimeSup = vk.step ~/ totalStepsSub;
    final keyTimeSub = vk.step % totalStepsSub;
    return (await kesSum.verify(signature.superSignature, signature.subRoot,
            VerificationKeyKesSum(value: vk.value, step: keyTimeSup))) &&
        (await kesSum.verify(signature.subSignature, message,
            VerificationKeyKesSum(value: signature.subRoot, step: keyTimeSub)));
  }

  Future<SecretKeyKesProduct> update(SecretKeyKesProduct sk, int step) async {
    if (step == 0) return sk;
    final keyTime = await getCurrentStep(sk);
    final keyTimeSup = await kesSum.getCurrentStep(sk.superTree);
    final heightSup = kesHelper.getTreeHeight(sk.superTree);
    final heightSub = kesHelper.getTreeHeight(sk.subTree);
    final totalSteps = kesHelper.exp(heightSup + heightSub);
    final totalStepsSub = kesHelper.exp(heightSub);
    final newKeyTimeSup = step ~/ totalStepsSub;
    final newKeyTimeSub = step % totalStepsSub;

    getSeed(Tuple2<List<int>, List<int>> seeds, int iter) async {
      if (iter < newKeyTimeSup) {
        final out = getSeed(await kesHelper.prng(seeds.second), iter + 1);
        _overwriteBytes(seeds.first);
        _overwriteBytes(seeds.second);
        return out;
      } else
        return seeds;
    }

    if (step > keyTime && step < totalSteps) {
      if (keyTimeSup < newKeyTimeSup) {
        await kesSum._eraseOldNode(sk.subTree);
        final seeds =
            await getSeed(Tuple2(List.empty(), sk.nextSubSeed), keyTimeSup);
        final superScheme =
            await kesSum._evolveKey(sk.superTree, newKeyTimeSub);
        final newSubScheme =
            await kesSum.generateSecretKey(seeds.first, heightSub);
        _overwriteBytes(seeds.first);
        final kesVkSub = await kesSum.generateVerificationKey(newSubScheme);
        final kesSigSuper = await kesSum.sign(superScheme, kesVkSub.value);
        final forwardSecureSuperScheme = _eraseLeafSecretKey(superScheme);
        final updatedSubScheme =
            await kesSum._evolveKey(newSubScheme, newKeyTimeSub);
        return SecretKeyKesProduct(
          superTree: forwardSecureSuperScheme,
          subTree: updatedSubScheme,
          nextSubSeed: seeds.second,
          subSignature: kesSigSuper,
          offset: sk.offset, // TODO
        );
      } else {
        final subScheme = await kesSum.update(sk.subTree, newKeyTimeSub);
        return SecretKeyKesProduct(
          superTree: sk.superTree,
          subTree: subScheme,
          nextSubSeed: sk.nextSubSeed,
          subSignature: sk.subSignature,
          offset: sk.offset, // TODO
        );
      }
    }
    throw Exception(
        "Update error - Max steps: $totalSteps, current step: $keyTime, requested increase: $step");
  }

  Future<int> getCurrentStep(SecretKeyKesProduct sk) async {
    final numSubSteps = kesHelper.exp(kesHelper.getTreeHeight(sk.subTree));
    final tSup = await kesSum.getCurrentStep(sk.superTree);
    final tSub = await kesSum.getCurrentStep(sk.subTree);
    return (tSup * numSubSteps) + tSub;
  }

  Future<SecretKeyKesProduct> generateSecretKey(
      List<int> seed, TreeHeight height) async {
    final rSuper = await kesHelper.prng(seed);
    final rSub = await kesHelper.prng(rSuper.second);
    final superScheme =
        await kesSum.generateSecretKey(rSuper.first, height.sup);
    final subScheme = await kesSum.generateSecretKey(rSub.first, height.sub);
    final kesVkSub = await kesSum.generateVerificationKey(subScheme);
    final kesSigSuper = await kesSum.sign(superScheme, kesVkSub.value);
    _overwriteBytes(rSuper.second);
    _overwriteBytes(seed);
    return SecretKeyKesProduct(
      superTree: superScheme,
      subTree: subScheme,
      nextSubSeed: rSub.second,
      subSignature: kesSigSuper,
      offset: Int64(0), // TODO
    );
  }

  Future<VerificationKeyKesProduct> generateVerificationKey(
      SecretKeyKesProduct sk) async {
    final superTree = sk.superTree;
    if (superTree is KesMerkleNode) {
      return VerificationKeyKesProduct(
          value: await kesHelper.witness(sk.superTree),
          step: await getCurrentStep(sk));
    } else if (superTree is KesSigningLeaf) {
      return VerificationKeyKesProduct(
          value: await kesHelper.witness(sk.superTree), step: 0);
    } else {
      return VerificationKeyKesProduct(
          value: List.filled(32, 0x00, growable: false), step: 0);
    }
  }

  _eraseLeafSecretKey(KesBinaryTree tree) {
    if (tree is KesMerkleNode) {
      if (tree.left is KesEmpty)
        return KesMerkleNode(tree.seed, tree.witnessLeft, tree.witnessRight,
            KesEmpty(), _eraseLeafSecretKey(tree.right));
      else if (tree.right is KesEmpty) {
        return KesMerkleNode(tree.seed, tree.witnessLeft, tree.witnessRight,
            _eraseLeafSecretKey(tree.left), KesEmpty());
      }
    } else if (tree is KesSigningLeaf) {
      _overwriteBytes(tree.sk);
      return KesSigningLeaf(List.filled(32, 0x00, growable: false), tree.vk);
    }
    throw Exception("Evolving Key Configuration Error");
  }
}

const kesProduct = KesProduct();

class TreeHeight {
  final int sup;
  final int sub;

  TreeHeight(this.sup, this.sub);
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

  factory SecretKeyKesProduct.decode(List<int> bytes) {
    final superTreeRes = _decodeTree(bytes);
    final subTreeRes = _decodeTree(superTreeRes.second);
    final nextSubSeed = subTreeRes.second.sublist(0, 32);
    final subSignatureRes = _decodeSignature(subTreeRes.second.sublist(32));
    final offset = Int64.fromBytes(subSignatureRes.second.sublist(0, 8));
    return SecretKeyKesProduct(
      superTree: superTreeRes.first,
      subTree: subTreeRes.first,
      nextSubSeed: nextSubSeed,
      subSignature: subSignatureRes.first,
      offset: offset,
    );
  }

  List<int> get encode {
    return [
      ..._encodeTree(superTree),
      ..._encodeTree(subTree),
      ...nextSubSeed,
      ..._encodeSignature(subSignature),
      ...offset.toBytes()
    ];
  }

  List<int> _encodeTree(KesBinaryTree tree) {
    if (tree is KesMerkleNode) {
      return [
        ...[0x00],
        ...tree.seed,
        ...tree.witnessLeft,
        ...tree.witnessRight,
        ..._encodeTree(tree.left),
        ..._encodeTree(tree.right)
      ];
    } else if (tree is KesSigningLeaf) {
      return [
        ...[0x01],
        ...tree.sk,
        ...tree.vk
      ];
    } else if (tree is KesEmpty) {
      return [0x02];
    }
    throw Exception("Encoding Error");
  }

  List<int> _encodeSignature(SignatureKesSum signature) {
    return [
      ...signature.verificationKey,
      ...signature.signature,
      ...Int64(signature.witness.length).toBytes(),
      ...signature.witness.flatMap((t) => t)
    ];
  }

  static Tuple2<KesBinaryTree, List<int>> _decodeTree(List<int> bytes) {
    int cursor = 1;
    if (bytes[0] == 0x00) {
      final seed = bytes.sublist(cursor, cursor += 32);
      final witnessLeft = bytes.sublist(cursor, cursor += 32);
      final witnessRight = bytes.sublist(cursor, cursor += 32);
      final left = _decodeTree(bytes.sublist(cursor));
      final right = _decodeTree(left.second);
      return Tuple2(
          KesMerkleNode(
              seed, witnessLeft, witnessRight, left.first, right.first),
          right.second);
    } else if (bytes[0] == 0x01) {
      final sk = bytes.sublist(cursor, cursor += 32);
      final vk = bytes.sublist(cursor, cursor += 32);
      return Tuple2(KesSigningLeaf(sk, vk), bytes.sublist(cursor));
    } else if (bytes[0] == 0x02) {
      return Tuple2(KesEmpty(), bytes.sublist(1));
    }
    throw Exception("Decoding Error");
  }

  static Tuple2<SignatureKesSum, List<int>> _decodeSignature(List<int> bytes) {
    int cursor = 0;
    final vk = bytes.sublist(cursor, cursor += 32);
    final signature = bytes.sublist(cursor, cursor += 64);
    Int64 witnessLength = _parseInt(bytes.sublist(cursor, cursor += 8));
    final witness = List.generate(witnessLength.toInt(), (index) {
      return bytes.sublist(cursor, cursor += 32);
    });

    final kesSignature = SignatureKesSum(
        verificationKey: vk, signature: signature, witness: witness);
    return Tuple2(kesSignature, bytes.sublist(cursor));
  }

  static Int64 _parseInt(List<int> bytes) {
    return Int64.fromBytes(bytes);
  }
}

class KeyPairKesProduct {
  final SecretKeyKesProduct sk;
  final VerificationKeyKesProduct vk;

  KeyPairKesProduct({required this.sk, required this.vk});
}
