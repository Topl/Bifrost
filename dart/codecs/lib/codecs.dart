import 'package:bifrost_common/models/unsigned.dart';
import 'package:bifrost_common/utils.dart';
import 'package:cryptography/dart.dart';
import 'package:fast_base58/fast_base58.dart';
import 'package:fpdart/fpdart.dart';
import 'package:topl_protobuf/brambl/models/identifier.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/io_transaction.pb.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/eligibility_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:fixnum/fixnum.dart';

const _blake2b256 = const DartBlake2b();

extension IoTransactionCodecs on IoTransaction {
  Identifier_IoTransaction32 get id => throw UnimplementedError();
}

extension BlockHeaderCodecs on BlockHeader {
  List<int> get immutableBytes => <int>[]
    ..addAll(parentHeaderId.value)
    ..addAll(parentSlot.toBytes())
    ..addAll(txRoot)
    ..addAll(bloomFilter)
    ..addAll(timestamp.immutableBytes)
    ..addAll(height.immutableBytes)
    ..addAll(slot.immutableBytes)
    ..addAll(eligibilityCertificate.immutableBytes)
    ..addAll(operationalCertificate.immutableBytes)
    ..addAll(metadata)
    ..addAll(address.value);
  BlockId get id => BlockId(value: _blake2b256.hashSync(immutableBytes).bytes);
}

extension UnsignedBlockHeaderCodecs on UnsignedBlockHeader {
  List<int> get signableBytes => <int>[]
    ..addAll(parentHeaderId.value)
    ..addAll(parentSlot.toBytes())
    ..addAll(txRoot)
    ..addAll(bloomFilter)
    ..addAll(timestamp.immutableBytes)
    ..addAll(height.immutableBytes)
    ..addAll(slot.immutableBytes)
    ..addAll(eligibilityCertificate.immutableBytes)
    ..addAll(partialOperationalCertificate.immutableBytes)
    ..addAll(metadata)
    ..addAll(address.value);
}

extension EligibilityCertificateCodecs on EligibilityCertificate {
  List<int> get immutableBytes => <int>[]
    ..addAll(vrfSig)
    ..addAll(vrfVK)
    ..addAll(thresholdEvidence)
    ..addAll(eta);
}

extension PartialOperationalCertificateCodecs on PartialOperationalCertificate {
  List<int> get immutableBytes => <int>[]
    ..addAll(parentVK.immutableBytes)
    ..addAll(parentSignature.immutableBytes)
    ..addAll(childVK);
}

extension OperationalCertificateCodecs on OperationalCertificate {
  List<int> get immutableBytes => <int>[]
    ..addAll(parentVK.immutableBytes)
    ..addAll(parentSignature.immutableBytes)
    ..addAll(childVK)
    ..addAll(childSignature);
}

extension VerificationKeyKesProductCodecs on VerificationKeyKesProduct {
  List<int> get immutableBytes => <int>[]
    ..addAll(value)
    ..addAll(step.immutableBytes);
}

extension IterableCodecs<T> on Iterable<T> {
  List<int> immutableBytes(List<int> Function(T) encodeItem) => <int>[]
    ..addAll(length.immutableBytes)
    ..addAll(flatMap((t) => encodeItem(t)));
}

extension SignatureKesSumCodecs on SignatureKesSum {
  List<int> get immutableBytes => <int>[]
    ..addAll(verificationKey)
    ..addAll(signature)
    ..addAll(witness.immutableBytes((t) => t));
}

extension SignatureKesProductCodecs on SignatureKesProduct {
  List<int> get immutableBytes => <int>[]
    ..addAll(superSignature.immutableBytes)
    ..addAll(subSignature.immutableBytes)
    ..addAll(subRoot);
}

extension IntCodecs on int {
  List<int> get immutableBytes => BigInt.from(this).bytes;
}

extension Int64Codecs on Int64 {
  List<int> get immutableBytes => toBigInt.bytes;
}

extension Int128Codecs on List<int> {
  String get base58 => Base58Encode(this);
}

extension BlockIdCodecs on BlockId {
  String get show => this.value.base58;
}

extension TransactionIdCodecs on Identifier_IoTransaction32 {
  String get show => this.evidence.digest.value.base58;
}
