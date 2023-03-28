import 'dart:async';
import 'dart:math';

import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_common/models/unsigned.dart';
import 'package:bifrost_minting/algebras/block_packer_algebra.dart';
import 'package:bifrost_minting/algebras/block_producer_algebra.dart';
import 'package:bifrost_common/algebras/clock_algebra.dart';
import 'package:bifrost_minting/algebras/staking_algebra.dart';
import 'package:async/async.dart';
import 'package:bifrost_minting/models/vrf_hit.dart';
import 'package:fixnum/fixnum.dart';
import 'package:logging/logging.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';

class BlockProducer extends BlockProducerAlgebra {
  final Stream<SlotData> parentHeaders;
  final StakingAlgebra staker;
  final ClockAlgebra clock;
  final BlockPackerAlgebra blockPacker;

  BlockProducer(this.parentHeaders, this.staker, this.clock, this.blockPacker);

  final log = Logger("BlockProducer");

  @override
  Stream<FullBlock> get blocks {
    final transformer =
        StreamTransformer((Stream<SlotData> stream, cancelOnError) {
      CancelableOperation<FullBlock?>? currentOperation = null;
      final controller = StreamController<FullBlock>(sync: true);
      controller.onListen = () {
        final subscription = stream.listen((data) {
          currentOperation?.cancel();
          final nextOp = _makeChild(data);
          currentOperation = nextOp;
          unawaited(nextOp.valueOrCancellation(null).then((blockOpt) {
            if (blockOpt != null) controller.add(blockOpt);
          }));
        },
            onError: controller.addError,
            onDone: controller.close,
            cancelOnError: cancelOnError);
        controller
          ..onPause = subscription.pause
          ..onResume = subscription.resume
          ..onCancel = subscription.cancel;
      };
      return controller.stream.listen(null);
    });
    return parentHeaders.transform(transformer);
  }

  CancelableOperation<FullBlock?> _makeChild(SlotData parentSlotData) {
    late final CancelableOperation<FullBlock?> completer;

    Future<FullBlock?> go() async {
      final nextHit = await _nextEligibility(parentSlotData.slotId);
      if (nextHit != null) {
        log.info("Packing block for slot=${nextHit.slot}");
        final bodyOpt = await _prepareBlockBody(
          parentSlotData,
          nextHit.slot,
          () => completer.isCanceled,
        );
        if (bodyOpt != null) {
          log.info("Constructing block for slot=${nextHit.slot}");
          final timestamp = clock.slotToTimestamps(nextHit.slot).first;
          final maybeHeader = await staker.certifyBlock(
              parentSlotData.slotId,
              nextHit.slot,
              _prepareUnsignedBlock(
                parentSlotData,
                bodyOpt,
                timestamp,
                nextHit,
              ));
          if (maybeHeader != null) {
            log.info(
                "Produced block header id=${maybeHeader.id.show} height=${maybeHeader.height} slot=${maybeHeader.slot} parentId=${maybeHeader.parentHeaderId.show}");
            return FullBlock(header: maybeHeader, fullBody: bodyOpt);
          } else {
            log.warning("Failed to produce block at next slot=${nextHit.slot}");
          }
        }
      }
      return null;
    }

    completer = CancelableOperation.fromFuture(go());
    return completer;
  }

  Future<VrfHit?> _nextEligibility(SlotId parentSlotId) async {
    Int64 test = parentSlotId.slot + 1;
    final globalSlot = clock.globalSlot;
    if (globalSlot > test) test = globalSlot;
    final exitSlot = test + 100000;
    VrfHit? maybeHit;
    while (maybeHit == null && test < exitSlot) {
      maybeHit = await staker.elect(parentSlotId, test);
      test = test + 1;
    }
    return maybeHit;
  }

  Future<FullBlockBody?> _prepareBlockBody(SlotData parentSlotData,
      Int64 targetSlot, bool Function() isCanceled) async {
    FullBlockBody body = FullBlockBody();
    bool c = isCanceled();
    while (!c && clock.globalSlot <= targetSlot) {
      final updated = await blockPacker.improvePackedBlock(body);
      if (updated != null)
        body = updated;
      else
        await Future.delayed(Duration(milliseconds: 100));
      c = isCanceled();
    }
    if (c) return null;
    return body;
  }

  UnsignedBlockHeader Function(PartialOperationalCertificate)
      _prepareUnsignedBlock(SlotData parentSlotData, FullBlockBody fullBody,
              Int64 timestamp, VrfHit nextHit) =>
          (PartialOperationalCertificate partialOperationalCertificate) =>
              UnsignedBlockHeader(
                parentSlotData.slotId.blockId,
                parentSlotData.slotId.slot,
                [], // TODO
                [], // TODO
                timestamp,
                parentSlotData.height + 1,
                nextHit.slot,
                nextHit.cert,
                partialOperationalCertificate,
                [],
                staker.address,
              );
}
