import 'package:bifrost_common/algebras/ClockAlgebra.dart';
import 'package:bifrost_common/models/ratio.dart';
import 'package:bifrost_crypto/Ed25519.dart';
import 'package:bifrost_minting/algebras/LeaderElectionMintingAlgebra.dart';
import 'package:bifrost_minting/algebras/operational_key_maker_algebra.dart';
import 'package:bifrost_minting/algebras/staking_algebra.dart';
import 'package:bifrost_minting/algebras/vrf_calculator_algebra.dart';
import 'package:bifrost_minting/models/vrf_hit.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';

class Staker extends StakingAlgebra {
  final StakingAddress _address;
  final LeaderElectionMintingAlgebra _leaderElection;
  final OperationalKeyMakerAlgebra _operationalKeys;
  // TODO: ConsensusValidationStateAlgebra
  // TODO: EtaCalculation
  final VrfCalculatorAlgebra _vrfProofs;
  final ClockAlgebra _clock;

  Staker(this._address, this._leaderElection, this._operationalKeys,
      this._vrfProofs, this._clock);

  @override
  StakingAddress get address => _address;

  @override
  Future<BlockHeader?> certifyBlock(
      SlotId parentSlotId,
      Int64 slot,
      UnsignedBlock Function(PartialOperationalCertificate p1)
          unsignedBlockBuilder) async {
    final operationalKeyOutOpt =
        await _operationalKeys.operationalKeyForSlot(slot, parentSlotId);
    if (operationalKeyOutOpt != null) {
      final partialCertificate = PartialOperationalCertificate(
        parentVK: operationalKeyOutOpt.parentVK,
        parentSignature: operationalKeyOutOpt.parentSignature,
        childVK: operationalKeyOutOpt.childKeyPair.vk,
      );
      final unsignedBlock = unsignedBlockBuilder(partialCertificate);
      final List<int> messageToSign = []; // TODO
      final operationalCertificate = OperationalCertificate(
        parentVK: operationalKeyOutOpt.parentVK,
        parentSignature: operationalKeyOutOpt.parentSignature,
        childVK: operationalKeyOutOpt.childKeyPair.vk,
        childSignature: await Ed25519.sign(
            messageToSign, operationalKeyOutOpt.childKeyPair),
      );
      final header = BlockHeader(
        parentHeaderId: unsignedBlock.unsignedHeader.parentHeaderId,
        parentSlot: unsignedBlock.unsignedHeader.parentSlot,
        txRoot: unsignedBlock.unsignedHeader.txRoot,
        bloomFilter: unsignedBlock.unsignedHeader.bloomFilter,
        timestamp: unsignedBlock.unsignedHeader.timestamp,
        height: unsignedBlock.unsignedHeader.height,
        slot: unsignedBlock.unsignedHeader.slot,
        eligibilityCertificate:
            unsignedBlock.unsignedHeader.eligibilityCertificate,
        operationalCertificate: operationalCertificate,
        metadata: unsignedBlock.unsignedHeader.metadata,
        address: unsignedBlock.unsignedHeader.address,
      );
      return header;
    }
  }

  @override
  Future<VrfHit?> elect(SlotId parentSlotId, Int64 slot) async {
    final spe = _clock.slotsPerEpoch;
    // TODO
    final eta = <int>[];
    final relativeStake = Ratio(BigInt.one, BigInt.one);
    return await _leaderElection.getHit(
        relativeStake, slot, slot - parentSlotId.slot, eta);
  }
}
