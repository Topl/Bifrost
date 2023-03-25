import 'package:bifrost_common/algebras/event_sourced_state_algebra.dart';
import 'package:bifrost_common/algebras/parent_child_tree_algebra.dart';
import 'package:bifrost_common/algebras/store_algebra.dart';
import 'package:bifrost_common/interpreters/event_tree_state.dart';
import 'package:bifrost_common/utils.dart';
import 'package:fpdart/fpdart.dart';
import 'package:topl_protobuf/brambl/models/identifier.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/io_transaction.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';

class ConsensusData {
  final StoreAlgebra<StakingAddress, BigInt> operatorStakes;
  final StoreAlgebra<void, BigInt> totalActiveStake;
  final StoreAlgebra<StakingAddress, SignatureKesProduct> registrations;

  ConsensusData(this.operatorStakes, this.totalActiveStake, this.registrations);
}

EventSourcedStateAlgebra<ConsensusData, BlockId> consensusDataEventSourcedState(
    BlockId initialBlockId,
    ParentChildTree<BlockId> parentChildTree,
    Future<void> Function(BlockId) currentEventChanged,
    ConsensusData initialState,
    Future<BlockBody> Function(BlockId) fetchBlockBody,
    Future<IoTransaction> Function(Identifier_IoTransaction32)
        fetchTransaction) {
  return EventTreeState(
      _applyBlock(fetchBlockBody, fetchTransaction),
      _unapplyBlock(fetchBlockBody, fetchTransaction),
      parentChildTree,
      initialState,
      initialBlockId,
      currentEventChanged);
}

_applyBlock(
    Future<BlockBody> Function(BlockId) fetchBlockBody,
    Future<IoTransaction> Function(Identifier_IoTransaction32)
        fetchTransaction) async {
  calculateStakeChanges(IoTransaction transaction) {
    final inputChanges = transaction.inputs
        .map((i) => i.value)
        .where((v) => v.hasTopl())
        .map((v) => v.topl)
        .map((t) => Tuple2(t.stakingAddress, -t.quantity.toBigInt));
    final outputChanges = transaction.outputs
        .map((i) => i.value)
        .where((v) => v.hasTopl())
        .map((v) => v.topl)
        .map((t) => Tuple2(t.stakingAddress, t.quantity.toBigInt));
    return <Tuple2<StakingAddress, BigInt>>[]
      ..addAll(inputChanges)
      ..addAll(outputChanges);
  }

  calculateRegistrationChanges(IoTransaction transaction) {
    final inputChanges = transaction.inputs
        .map((i) => i.value)
        .where((v) => v.hasRegistration())
        .map((v) => v.registration)
        .map((t) => MapEntry<StakingAddress, SignatureKesProduct?>(
            t.stakingAddress, null));
    final outputChanges = transaction.outputs
        .map((i) => i.value)
        .where((v) => v.hasRegistration())
        .map((v) => v.registration)
        .map((t) => MapEntry<StakingAddress, SignatureKesProduct?>(
            t.stakingAddress, t.registration));
    return Map.fromEntries(
      <MapEntry<StakingAddress, SignatureKesProduct?>>[]
        ..addAll(inputChanges)
        ..addAll(outputChanges),
    );
  }

  apply(ConsensusData state, BlockId blockId) async {
    final body = await fetchBlockBody(blockId);
    final transactions =
        await Future.wait(body.transactionIds.map(fetchTransaction));
    final stakeChanges = transactions.flatMap(calculateStakeChanges);
    final registrationChanges = Map.fromEntries(transactions
        .map(calculateRegistrationChanges)
        .flatMap((m) => m.entries));
    final previousTotalStake = await state.totalActiveStake.getOrRaise({});
    final newTotalStake = stakeChanges.foldLeft<BigInt>(
        previousTotalStake, (a, b) => a + b.second);
    await state.totalActiveStake.put({}, newTotalStake);
    for (final stakeChange in stakeChanges) {
      final previousStake = await state.operatorStakes.get(stakeChange.first);
      final newStake = previousStake != null
          ? previousStake + stakeChange.second
          : stakeChange.second;
      await state.operatorStakes.put(stakeChange.first, newStake);
    }
    for (final registrationChange in registrationChanges.entries) {
      if (registrationChange.value != null)
        await state.registrations
            .put(registrationChange.key, registrationChange.value!);
      else
        await state.registrations.remove(registrationChange.key);
    }
    return state;
  }

  return apply;
}

_unapplyBlock(
    Future<BlockBody> Function(BlockId) fetchBlockBody,
    Future<IoTransaction> Function(Identifier_IoTransaction32)
        fetchTransaction) async {
  calculateStakeChanges(IoTransaction transaction) {
    final outputChanges = transaction.outputs
        .map((i) => i.value)
        .where((v) => v.hasTopl())
        .map((v) => v.topl)
        .map((t) => Tuple2(t.stakingAddress, -t.quantity.toBigInt));
    final inputChanges = transaction.inputs
        .map((i) => i.value)
        .where((v) => v.hasTopl())
        .map((v) => v.topl)
        .map((t) => Tuple2(t.stakingAddress, t.quantity.toBigInt));
    return <Tuple2<StakingAddress, BigInt>>[]
      ..addAll(outputChanges)
      ..addAll(inputChanges);
  }

  calculateRegistrationChanges(IoTransaction transaction) {
    final outputChanges = transaction.outputs
        .map((i) => i.value)
        .where((v) => v.hasRegistration())
        .map((v) => v.registration)
        .map((t) => MapEntry<StakingAddress, SignatureKesProduct?>(
            t.stakingAddress, null));
    final inputChanges = transaction.inputs
        .map((i) => i.value)
        .where((v) => v.hasRegistration())
        .map((v) => v.registration)
        .map((t) => MapEntry<StakingAddress, SignatureKesProduct?>(
            t.stakingAddress, t.registration));
    return Map.fromEntries(
      <MapEntry<StakingAddress, SignatureKesProduct?>>[]
        ..addAll(outputChanges)
        ..addAll(inputChanges),
    );
  }

  f(ConsensusData state, BlockId blockId) async {
    final body = await fetchBlockBody(blockId);
    final transactions =
        await Future.wait(body.transactionIds.reversed.map(fetchTransaction));
    final stakeChanges = transactions.flatMap(calculateStakeChanges);
    final registrationChanges = Map.fromEntries(transactions
        .map(calculateRegistrationChanges)
        .flatMap((m) => m.entries));
    final previousTotalStake = await state.totalActiveStake.getOrRaise({});
    final newTotalStake = stakeChanges.foldLeft<BigInt>(
        previousTotalStake, (a, b) => a + b.second);
    await state.totalActiveStake.put({}, newTotalStake);
    for (final stakeChange in stakeChanges) {
      final previousStake = await state.operatorStakes.get(stakeChange.first);
      final newStake = previousStake != null
          ? previousStake + stakeChange.second
          : stakeChange.second;
      await state.operatorStakes.put(stakeChange.first, newStake);
    }
    for (final registrationChange in registrationChanges.entries) {
      if (registrationChange.value != null)
        await state.registrations
            .put(registrationChange.key, registrationChange.value!);
      else
        await state.registrations.remove(registrationChange.key);
    }
    return state;
  }

  return f;
}
