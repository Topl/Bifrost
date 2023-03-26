import 'dart:convert';
import 'dart:typed_data';

import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_common/models/common.dart';
import 'package:bifrost_common/utils.dart';
import 'package:bifrost_crypto/ed25519vrf.dart';
import 'package:hashlib/hashlib.dart';
import 'package:rational/rational.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';

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

extension BlockHeaderOps on BlockHeader {
  SlotData get slotData => SlotData(
        slotId: SlotId(blockId: id, slot: slot),
        parentSlotId: SlotId(blockId: parentHeaderId, slot: parentSlot),
        rho: ed25519Vrf.proofToHash(eligibilityCertificate.vrfSig),
        eta: eligibilityCertificate.eta,
      );
}
