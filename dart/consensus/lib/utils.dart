import 'dart:convert';
import 'dart:typed_data';

import 'package:bifrost_common/models/common.dart';
import 'package:bifrost_common/utils.dart';
import 'package:hashlib/hashlib.dart';
import 'package:rational/rational.dart';

final TestStringArray = utf8.encode("TEST");
final NonceStringArray = utf8.encode("NONCE");

extension RhoOps on Rho {
  List<int> get rhoTestHash =>
      blake2b512.convert(this..addAll(TestStringArray)).bytes;
  List<int> get rhoNonceHash =>
      blake2b512.convert(this..addAll(NonceStringArray)).bytes;
}

extension RatioOps on Rational {
  Uint8List get thresholdEvidence =>
      blake2b256.convert(numerator.bytes..addAll(denominator.bytes)).bytes;
}
