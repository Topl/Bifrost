import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_common/algebras/clock_algebra.dart';
import 'package:bifrost_common/utils.dart';
import 'package:bifrost_consensus/algebras/block_header_validation_algebra.dart';
import 'package:bifrost_consensus/algebras/consensus_validation_state_algebra.dart';
import 'package:bifrost_consensus/algebras/eta_calculation_algebra.dart';
import 'package:bifrost_consensus/algebras/leader_election_validation_algebra.dart';
import 'package:bifrost_consensus/models/vrf_argument.dart';
import 'package:bifrost_consensus/utils.dart';
import 'package:bifrost_crypto/ed25519.dart';
import 'package:bifrost_crypto/ed25519vrf.dart';
import 'package:bifrost_crypto/kes.dart';
import 'package:bifrost_crypto/utils.dart';
import 'package:fpdart/fpdart.dart';
import 'package:hashlib/hashlib.dart';
import 'package:rational/rational.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';

class BlockHeaderValidation extends BlockHeadervalidationAlgebra {
  final BlockId genesisBlockId;
  final EtaCalculationAlgebra etaInterpreter;
  final ConsensusValidationStateAlgebra consensusValidationState;
  final LeaderElectionValidationAlgebra leaderElectionValidation;
  final ClockAlgebra clock;
  final Future<BlockHeader> Function(BlockId) fetchHeader;

  BlockHeaderValidation(
      this.genesisBlockId,
      this.etaInterpreter,
      this.consensusValidationState,
      this.leaderElectionValidation,
      this.clock,
      this.fetchHeader);

  @override
  Future<List<String>> validate(BlockHeader header) async {
    if (header.id == genesisBlockId) return [];
    final parent = await fetchHeader(header.parentHeaderId);
    final List<String> errors = [];

    errors.addAll(_statelessVerification(header, parent));
    if (errors.isNotEmpty) return errors;

    errors.addAll(_timeSlotVerification(header));
    if (errors.isNotEmpty) return errors;

    errors.addAll(await _vrfVerification(header));
    if (errors.isNotEmpty) return errors;

    // TODO
    // errors.addAll(await _kesVerification(header));
    // if (errors.isNotEmpty) return errors;

    // errors.addAll(await _registrationVerification(header));
    // if (errors.isNotEmpty) return errors;

    final vrfThresholdOrErrors = await _vrfThresholdFor(header);

    if (vrfThresholdOrErrors.isLeft())
      return vrfThresholdOrErrors.getLeft().toNullable()!;

    final vrfThreshold = vrfThresholdOrErrors.getRight().toNullable()!;

    errors.addAll(_vrfThresholdVerification(header, vrfThreshold));
    if (errors.isNotEmpty) return errors;

    errors.addAll(await _eligibilityVerification(header, vrfThreshold));
    if (errors.isNotEmpty) return errors;

    return [];
  }

  List<String> _statelessVerification(BlockHeader child, BlockHeader parent) {
    if (child.slot <= parent.slot) return ["NonForwardSlot"];
    if (child.timestamp <= parent.timestamp) return ["NonForwardTimestamp"];
    if (child.height != parent.height + 1) return ["NonForwardHeight"];
    return [];
  }

  List<String> _timeSlotVerification(BlockHeader header) {
    if (clock.timestampToSlot(header.timestamp) != header.slot)
      return ["TimestampSlotMismatch"];
    if (header.slot > (clock.globalSlot + clock.forwardBiasedSlotWindow))
      return ["SlotBeyondForwardBiasedSlotWindow"];
    return [];
  }

  Future<List<String>> _vrfVerification(BlockHeader header) async {
    final expectedEta = await etaInterpreter.etaToBe(
        SlotId(slot: header.parentSlot, blockId: header.parentHeaderId),
        header.slot);
    if (expectedEta != header.eligibilityCertificate.eta)
      return ["InvalidEligibilityCertificate"];
    final signatureVerification = await ed25519Vrf.verify(
      header.eligibilityCertificate.vrfSig,
      VrfArgument(expectedEta, header.slot).signableBytes,
      header.eligibilityCertificate.vrfVK,
    );
    if (!signatureVerification) return ["InvalidEligibilityCertificateEta"];
    return [];
  }

  Future<List<String>> _kesVerification(BlockHeader header) async {
    final parentCommitmentVerification = await kesProduct.verify(
        header.operationalCertificate.parentSignature,
        []
          ..addAll(header.operationalCertificate.childVK)
          ..addAll(header.slot.immutableBytes),
        header.operationalCertificate.parentVK);
    if (!parentCommitmentVerification)
      return ["InvalidOperationalParentSignature"];
    final childSignatureResult = await ed25519.verify(
        header.operationalCertificate.childSignature,
        header.unsigned.signableBytes,
        header.operationalCertificate.childVK);
    if (!childSignatureResult) return ["InvalidBlockProof"];
    return [];
  }

  Future<List<String>> _registrationVerification(BlockHeader header) async {
    final commitment = await consensusValidationState.operatorRegistration(
        header.id, header.slot, header.address);
    if (commitment == null) return ["Unregistered"];
    final message = blake2b256
        .convert([]
          ..addAll(header.eligibilityCertificate.vrfVK)
          ..addAll(header.address.value))
        .bytes
        .int8List;
    final verificationResult = await kesProduct.verify(
        commitment,
        message,
        VerificationKeyKesProduct(
            value: header.operationalCertificate.parentVK.value, step: 0));
    if (!verificationResult) return ["RegistrationCommitmentMismatch"];
    return [];
  }

  Future<Either<List<String>, Rational>> _vrfThresholdFor(
      BlockHeader header) async {
    final relativeStake = await consensusValidationState.operatorRelativeStake(
        header.id, header.slot, header.address);
    if (relativeStake == null) return Left(["Unregistered"]);
    final threshold = await leaderElectionValidation.getThreshold(
        relativeStake, header.slot - header.parentSlot);
    return Right(threshold);
  }

  List<String> _vrfThresholdVerification(
      BlockHeader header, Rational threshold) {
    final evidence = threshold.thresholdEvidence;
    if (!evidence.sameElements(header.eligibilityCertificate.thresholdEvidence))
      return ["InvalidVrfThreshold"];
    return [];
  }

  Future<List<String>> _eligibilityVerification(
      BlockHeader header, Rational threshold) async {
    final rho =
        await ed25519Vrf.proofToHash(header.eligibilityCertificate.vrfSig);
    final isSlotLeader =
        await leaderElectionValidation.isSlotLeaderForThreshold(threshold, rho);
    if (!isSlotLeader) return ["Ineligible"];
    return [];
  }
}
