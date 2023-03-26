import 'dart:typed_data';
import 'package:bifrost_common/models/unsigned.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/quivr/models/shared.pb.dart';

// Source: https://github.com/dart-lang/sdk/issues/32803#issuecomment-1228291047
extension BigIntOps on BigInt {
  Uint8List get bytes {
    BigInt number = this;
    // Not handling negative numbers. Decide how you want to do that.
    int bytes = (number.bitLength + 7) >> 3;
    var b256 = BigInt.from(256);
    var result = Uint8List(bytes);
    for (int i = 0; i < bytes; i++) {
      result[bytes - 1 - i] = number.remainder(b256).toInt();
      number = number >> 8;
    }
    return result;
  }
}

extension Int64Ops on Int64 {
  BigInt get toBigInt => toBytes().toBigInt;
}

extension ListBigIntOps on Iterable<int> {
  BigInt get toBigInt {
    BigInt result = BigInt.zero;

    for (final byte in this) {
      // reading in big-endian, so we essentially concat the new byte to the end
      result = (result << 8) | BigInt.from(byte & 0xff);
    }
    return result;
  }
}

extension Int128Ops on Int128 {
  BigInt get toBigInt => value.toBigInt;
}

extension BlockHeaderOps on BlockHeader {
  UnsignedBlockHeader get unsigned => UnsignedBlockHeader(
        parentHeaderId,
        parentSlot,
        txRoot,
        bloomFilter,
        timestamp,
        height,
        slot,
        eligibilityCertificate,
        PartialOperationalCertificate(
            operationalCertificate.parentVK,
            operationalCertificate.parentSignature,
            operationalCertificate.childVK),
        metadata,
        address,
      );
}
