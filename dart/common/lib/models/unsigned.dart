import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/eligibility_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';

class UnsignedBlockHeader {
  final BlockId parentHeaderId;
  final Int64 parentSlot;
  final List<int> txRoot;
  final List<int> bloomFilter;
  final Int64 timestamp;
  final Int64 height;
  final Int64 slot;
  final EligibilityCertificate eligibilityCertificate;
  final PartialOperationalCertificate partialOperationalCertificate;
  final List<int> metadata;
  final StakingAddress address;

  UnsignedBlockHeader(
      this.parentHeaderId,
      this.parentSlot,
      this.txRoot,
      this.bloomFilter,
      this.timestamp,
      this.height,
      this.slot,
      this.eligibilityCertificate,
      this.partialOperationalCertificate,
      this.metadata,
      this.address);
}

class PartialOperationalCertificate {
  final VerificationKeyKesProduct parentVK;
  final SignatureKesProduct parentSignature;
  final List<int> childVK;

  PartialOperationalCertificate(
      this.parentVK, this.parentSignature, this.childVK);
}
