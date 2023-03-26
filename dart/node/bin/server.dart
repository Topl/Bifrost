import 'dart:async';

import 'package:args/args.dart';
import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_common/interpreters/clock.dart';
import 'package:bifrost_common/interpreters/parent_child_tree.dart';
import 'package:bifrost_consensus/interpreters/block_header_validation.dart';
import 'package:bifrost_consensus/interpreters/chain_selection.dart';
import 'package:bifrost_consensus/interpreters/consensus_data_event_sourced_state.dart';
import 'package:bifrost_consensus/interpreters/consensus_validation_state.dart';
import 'package:bifrost_consensus/interpreters/epoch_boundaries.dart';
import 'package:bifrost_consensus/interpreters/eta_calculation.dart';
import 'package:bifrost_consensus/interpreters/leader_election_validation.dart';
import 'package:bifrost_consensus/interpreters/local_chain.dart';
import 'package:bifrost_consensus/models/vrf_config.dart';
import 'package:bifrost_consensus/utils.dart';
import 'package:bifrost_crypto/ed25519vrf.dart';
import 'package:bifrost_minting/interpreters/block_packer.dart';
import 'package:bifrost_minting/interpreters/block_producer.dart';
import 'package:bifrost_minting/interpreters/operational_key_maker.dart';
import 'package:bifrost_minting/interpreters/staking.dart';
import 'package:bifrost_minting/interpreters/vrf_calculator.dart';
import 'package:bifrost_node/data_stores.dart';
import 'package:fixnum/fixnum.dart';
import 'package:async/async.dart' show StreamGroup;
import 'package:rational/rational.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';

void main(List<String> args) async {
  print("Command args=$args");

  final appArgs = argParser.parse(args);
  print(
      "Parsed args: ${appArgs.options.map((n) => "$n=${appArgs[n]}").join(" ")}");

  final genesisBlock = FullBlock(); // TODO

  final genesisBlockId = genesisBlock.header.id;

  final operatorAddress = StakingAddress();

  final vrfKeyPair = await ed25519Vrf.generateKeyPair();

  final vrfVK = List.filled(32, 0);

  final clock = Clock(
    Duration(milliseconds: 500),
    ChainSelectionKLookback * 6,
    Int64(DateTime.now().millisecondsSinceEpoch),
    Int64(50),
  );

  final dataStores = await DataStores.init(genesisBlock);

  final currentEventIdGetterSetters =
      CurrentEventIdGetterSetters(dataStores.currentEventIds);

  final canonicalHeadId = await currentEventIdGetterSetters.canonicalHead.get();

  final parentChildTree = ParentChildTree<BlockId>(
      dataStores.parentChildTree.get,
      dataStores.parentChildTree.put,
      genesisBlock.header.parentHeaderId);

  final vrfConfig =
      VrfConfig(15, 40, Rational.fromInt(1, 20), Rational.fromInt(1, 2));

  final etaCalculation = EtaCalculation(dataStores.slotData.getOrRaise, clock,
      genesisBlock.header.eligibilityCertificate.eta);

  final leaderElectionValidation = LeaderElectionValidation(vrfConfig);

  final vrfCalculator = VrfCalculator(
      vrfKeyPair.sk, clock, leaderElectionValidation, vrfConfig, 512);

  final operationalKeyMaker = OperationalKeyMaker();

  final epochBoundaryState = epochBoundariesEventSourcedState(
      clock,
      genesisBlockId,
      parentChildTree,
      currentEventIdGetterSetters.epochBoundaries.set,
      dataStores.epochBoundaries,
      dataStores.slotData.getOrRaise);
  final consensusDataState = consensusDataEventSourcedState(
      genesisBlockId,
      parentChildTree,
      currentEventIdGetterSetters.consensusData.set,
      ConsensusData(dataStores.operatorStakes, dataStores.activeStake,
          dataStores.registrations),
      dataStores.bodies.getOrRaise,
      dataStores.transactions.getOrRaise);

  final consensusValidationState = ConsensusValidationState(
      genesisBlockId, epochBoundaryState, consensusDataState, clock);

  final localChain = LocalChain(genesisBlockId);

  final chainSelection = ChainSelection(dataStores.slotData.getOrRaise);

  final headerValidation = BlockHeaderValidation(
      genesisBlockId,
      etaCalculation,
      consensusValidationState,
      leaderElectionValidation,
      clock,
      dataStores.headers.getOrRaise);

  final staker = Staking(
    operatorAddress,
    vrfVK,
    operationalKeyMaker,
    consensusValidationState,
    etaCalculation,
    vrfCalculator,
    leaderElectionValidation,
  );

  final blockProducer = BlockProducer(
    StreamGroup.merge([
      Stream.value(genesisBlock.header.slotData),
      localChain.adoptions.asyncMap(dataStores.slotData.getOrRaise),
    ]),
    staker,
    clock,
    BlockPacker(),
  );

  final mintedBlocksStream = blockProducer.blocks;

  processBlock(FullBlock block) async {
    final headerValidationErrors =
        await headerValidation.validate(block.header);
    if (headerValidationErrors.isNotEmpty) {
      // TODO: throw?
      print("Invalid block.  reason=$headerValidationErrors");
    } else {
      final id = block.header.id;
      final body = BlockBody(
          transactionIds: block.fullBody.transaction.map((t) => t.id));

      await dataStores.slotData.put(id, block.header.slotData);
      await dataStores.headers.put(id, block.header);
      await dataStores.bodies.put(id, body);
      if (await chainSelection.select(id, await localChain.currentHead) == id) {
        print("Adopting id=$id");
        localChain.adopt(id);
      }
    }
  }

  unawaited(
    mintedBlocksStream
        .map((block) {
          print(
              "Minted block. id=${block.header.id.show} height=${block.header.height} slot=${block.header.slot}");
          return block;
        })
        .asyncMap(processBlock)
        .drain(),
  );
}

final ChainSelectionKLookback = Int64(50);

final argParser = ArgParser()
  ..addOption("p2p-bind-host", defaultsTo: "0.0.0.0")
  ..addOption("p2p-bind-port", defaultsTo: "9084")
  ..addOption("p2p-known-peers", defaultsTo: "");
