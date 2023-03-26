import 'dart:math';

import 'package:bifrost_crypto/kes.dart';
import 'package:cryptography/cryptography.dart';
import 'package:fpdart/fpdart.dart';

class KesHelper {
  const KesHelper();
  Future<List<int>> hash(List<int> input) async {
    return (await Blake2b().hash(input)).bytes;
  }

  int exp(int n) => pow(2, n).toInt();

  Future<Tuple2<List<int>, List<int>>> prng(List<int> seed) async {
    final r1 = await hash([0x00]..addAll(seed));
    final r2 = await hash([0x01]..addAll(seed));
    return Tuple2(r1, r2);
  }

  int getTreeHeight(KesBinaryTree tree) {
    int loop(KesBinaryTree t) {
      if (t is KesMerkleNode)
        return max(loop(t.left), loop(t.right)) + 1;
      else if (t is KesSigningLeaf)
        return 1;
      else
        return 0;
    }

    return loop(tree) - 1;
  }

  Future<List<int>> witness(KesBinaryTree tree) async {
    if (tree is KesMerkleNode)
      return hash(<int>[]
        ..addAll(tree.witnessLeft)
        ..addAll(tree.witnessRight));
    else if (tree is KesSigningLeaf)
      return hash(tree.vk);
    else
      return List.filled(32, 0x00, growable: false);
  }
}

const kesHelper = KesHelper();
