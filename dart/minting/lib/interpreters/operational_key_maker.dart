import 'dart:math';

import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_common/algebras/clock_algebra.dart';
import 'package:bifrost_common/models/common.dart';
import 'package:bifrost_consensus/algebras/consensus_validation_state_algebra.dart';
import 'package:bifrost_consensus/algebras/eta_calculation_algebra.dart';
import 'package:bifrost_crypto/ed25519.dart' show ed25519;
import 'package:bifrost_crypto/kes.dart';
import 'package:bifrost_minting/algebras/operational_key_maker_algebra.dart';
import 'package:bifrost_minting/algebras/secure_store_algebra.dart';
import 'package:bifrost_minting/algebras/vrf_calculator_algebra.dart';
import 'package:bifrost_minting/interpreters/vrf_calculator.dart';
import 'package:fixnum/fixnum.dart';
import 'package:fpdart/fpdart.dart';
import 'package:rational/rational.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';
import 'package:topl_protobuf/quivr/models/shared.pb.dart';

class OperationalKeyMaker extends OperationalKeyMakerAlgebra {
  final Int64 operationalPeriodLength;
  final Int64 activationOperationalPeriod;
  final StakingAddress address;
  final SecureStoreAlgebra secureStore;
  final ClockAlgebra clock;
  final VrfCalculatorAlgebra vrfCalculator;
  final EtaCalculationAlgebra etaCalculation;
  final ConsensusValidationStateAlgebra consensusValidationState;
  Int64 currentOperationalPeriod;
  Map<Int64, OperationalKeyOut>? _currentKeyCache;

  OperationalKeyMaker(
    this.operationalPeriodLength,
    this.activationOperationalPeriod,
    this.address,
    this.secureStore,
    this.clock,
    this.vrfCalculator,
    this.etaCalculation,
    this.consensusValidationState,
    this.currentOperationalPeriod,
    this._currentKeyCache,
  );

  static Future<OperationalKeyMaker> init(
    SlotId parentSlotId,
    Int64 operationalPeriodLength,
    Int64 activationOperationalPeriod,
    StakingAddress address,
    SecureStoreAlgebra secureStore,
    ClockAlgebra clock,
    VrfCalculatorAlgebra vrfCalculator,
    EtaCalculationAlgebra etaCalculation,
    ConsensusValidationStateAlgebra consensusValidationState,
    SecretKeyKesProduct initialSK,
  ) async {
    Int64 slot = clock.globalSlot;
    if (slot < 0) slot = Int64.ZERO;
    final operationalPeriod = slot ~/ operationalPeriodLength;
    final impl = OperationalKeyMaker(
      operationalPeriodLength,
      activationOperationalPeriod,
      address,
      secureStore,
      clock,
      vrfCalculator,
      etaCalculation,
      consensusValidationState,
      operationalPeriod,
      null,
    );

    await secureStore.write("", initialSK.encode);

    final relativeStake = await consensusValidationState.operatorRelativeStake(
        parentSlotId.blockId, slot, address);
    if (relativeStake != null) {
      final initialKeysOpt = await impl._consumeEvolvePersist(
          (operationalPeriod - activationOperationalPeriod).toInt(),
          (t) => impl._prepareOperationalPeriodKeys(
              t, slot, parentSlotId, relativeStake));
      impl._currentKeyCache = initialKeysOpt;
      impl.currentOperationalPeriod = operationalPeriod;
    }
    return impl;
  }

  @override
  Future<OperationalKeyOut?> operationalKeyForSlot(
      Int64 slot, SlotId parentSlotId) async {
    final operationalPeriod = slot ~/ operationalPeriodLength;
    if (operationalPeriod == currentOperationalPeriod)
      return _currentKeyCache?[slot];
    final relativeStake = await consensusValidationState.operatorRelativeStake(
        parentSlotId.blockId, slot, address);
    if (relativeStake == null) return null;
    final newKeys = await _consumeEvolvePersist(
        (operationalPeriod - activationOperationalPeriod).toInt(),
        (t) => _prepareOperationalPeriodKeys(
            t, slot, parentSlotId, relativeStake));
    currentOperationalPeriod = operationalPeriod;
    _currentKeyCache = newKeys;
    return newKeys?[slot];
  }

  Future<T?> _consumeEvolvePersist<T>(
      int timeStep, Future<T> Function(SecretKeyKesProduct) use) async {
    final fileNames = await secureStore.list();
    if (fileNames.length != 1)
      throw Exception("SecureStore contained invalid number of keys");
    final fileName = fileNames.first;
    final diskKeyBytes = await secureStore.consume(fileName);
    if (diskKeyBytes == null) return null;
    final SecretKeyKesProduct diskKey =
        SecretKeyKesProduct.decode(diskKeyBytes);
    final latest = await kesProduct.getCurrentStep(diskKey);
    SecretKeyKesProduct? currentPeriodKey;
    if (latest == timeStep)
      currentPeriodKey = diskKey;
    else if (latest > timeStep) {
      // TODO log
      secureStore.write(fileName, diskKeyBytes);
    } else {
      currentPeriodKey = await kesProduct.update(diskKey, timeStep);
    }

    if (currentPeriodKey == null) return null;
    final res = await use(currentPeriodKey);
    final nextTimeStep = timeStep + 1;
    final updated = await kesProduct.update(currentPeriodKey, nextTimeStep);
    await secureStore.write("k", updated.encode);
    return res;
  }

  Future<Map<Int64, OperationalKeyOut>> _prepareOperationalPeriodKeys(
      SecretKeyKesProduct kesParent,
      Slot fromSlot,
      SlotId parentSlotId,
      Rational relativeStake) async {
    final epoch = clock.epochOfSlot(fromSlot);
    final eta = await etaCalculation.etaToBe(parentSlotId, fromSlot);
    final operationalPeriod = fromSlot ~/ operationalPeriodLength;

    final operationalPeriodSlotMin =
        operationalPeriod * operationalPeriodLength;
    final operationalPeriodSlotMax =
        (operationalPeriod + 1) * operationalPeriodLength;
    final ineligibleSlots = await vrfCalculator.ineligibleSlots(
        epoch,
        eta,
        Tuple2(operationalPeriodSlotMin, operationalPeriodSlotMax),
        relativeStake);
    final slots = List.generate(
            (operationalPeriodLength - (fromSlot % operationalPeriodLength))
                .toInt(),
            (i) => fromSlot + i)
        .where((s) => !ineligibleSlots.contains(s))
        .toList();
    final outs = await _prepareOperationalPeriodKeysImpl(kesParent, slots);
    final mappedKeys =
        Map.fromEntries(outs.map((out) => MapEntry(out.slot, out)));
    return mappedKeys;
  }

  Future<List<OperationalKeyOut>> _prepareOperationalPeriodKeysImpl(
      SecretKeyKesProduct kesParent, List<Slot> slots) async {
    final result = <OperationalKeyOut>[];
    final parentVK = await kesProduct.generateVerificationKey(kesParent);
    for (int i = 0; i < slots.length; i++) {
      final slot = slots[i];
      final childKeyPair = await ed25519.generateKeyPair();
      final parentSignature = await kesProduct.sign(
          kesParent,
          <int>[]
            ..addAll(childKeyPair.vk)
            ..addAll(slot.immutableBytes));
      result.add(
          OperationalKeyOut(slot, childKeyPair, parentSignature, parentVK));
    }
    return result;
  }
}
