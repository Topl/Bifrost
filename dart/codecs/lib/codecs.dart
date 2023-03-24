import 'package:cryptography/dart.dart';
import 'dart:convert';
import 'package:fast_base58/fast_base58.dart';
import 'package:topl_protobuf/brambl/models/identifier.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/io_transaction.pb.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';

const _sha256 = const DartSha256();

extension IoTransactionCodecs on IoTransaction {
  Identifier_IoTransaction32 get id => throw UnimplementedError();
}

extension BlockHeaderCodecs on BlockHeader {
  // TODO
  List<int> get immutableBytes => <int>[]
    ..addAll(parentHeaderId.value)
    ..addAll(slot.toBytes())
    ..addAll(height.toBytes())
    ..addAll(timestamp.toBytes());
  BlockId get id => BlockId(value: _sha256.hashSync(immutableBytes).bytes);
}

extension Int128Codecs on List<int> {
  String get base58 => Base58Encode(this);
  // https://github.com/dart-lang/sdk/issues/32803#issuecomment-1228291047
  BigInt get bigInt {
    BigInt result = BigInt.zero;
    for (final byte in this) {
      // reading in big-endian, so we essentially concat the new byte to the end
      result = (result << 8) | BigInt.from(byte & 0xff);
    }
    return result;
  }
}

extension BlockIdCodecs on BlockId {
  String get show => this.value.base58;
}

extension TransactionIdCodecs on Identifier_IoTransaction32 {
  String get show => this.evidence.digest.value.base58;
}
