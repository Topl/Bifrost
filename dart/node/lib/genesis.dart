import 'dart:convert';

import 'package:bifrost_codecs/codecs.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/brambl/models/transaction/io_transaction.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/unspent_transaction_output.pb.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/eligibility_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';

class GenesisConfig {
  final Int64 timestamp;
  final List<UnspentTransactionOutput> outputs;
  final List<int> etaPrefix;

  GenesisConfig(this.timestamp, this.outputs, this.etaPrefix);

  static final DefaultEtaPrefix = utf8.encode("genesis");

  FullBlock get block {
    final transaction = IoTransaction(outputs: outputs);
    final transactions = [transaction];
    final eta = <int>[]
      ..addAll(etaPrefix)
      ..addAll(transaction.id.evidence.digest.value);
    final eligibilityCertificate = EligibilityCertificate(
      vrfSig: _emptyBytes(80),
      vrfVK: _emptyBytes(32),
      thresholdEvidence: _emptyBytes(32),
      eta: eta,
    );
    final header = BlockHeader(
      parentHeaderId: GenesisParentId,
      parentSlot: Int64(-1),
      txRoot: _emptyBytes(32), // TODO
      bloomFilter: _emptyBytes(256), // TODO
      timestamp: timestamp,
      height: Int64.ONE,
      slot: Int64.ZERO,
      eligibilityCertificate: eligibilityCertificate,
      operationalCertificate: GenesisOperationalCertificate,
      address: StakingAddress(value: _emptyBytes(32)),
    );

    return FullBlock(
      header: header,
      fullBody: FullBlockBody(transaction: transactions),
    );
  }
}

final GenesisParentId = BlockId(value: List.filled(32, 0x00, growable: false));
final GenesisOperationalCertificate = OperationalCertificate(
  parentVK: VerificationKeyKesProduct(value: _emptyBytes(32), step: 0),
  parentSignature: SignatureKesProduct(
    superSignature:
        SignatureKesSum(verificationKey: _emptyBytes(32), witness: []),
    subSignature:
        SignatureKesSum(verificationKey: _emptyBytes(32), witness: []),
    subRoot: _emptyBytes(32),
  ),
  childVK: _emptyBytes(32),
  childSignature: _emptyBytes(64),
);

_emptyBytes(int length) => List.filled(length, 0, growable: false);
