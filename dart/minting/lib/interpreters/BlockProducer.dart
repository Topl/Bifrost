import 'dart:async';

import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_minting/algebras/block_packer_algebra.dart';
import 'package:bifrost_minting/algebras/block_producer_algebra.dart';
import 'package:bifrost_common/algebras/ClockAlgebra.dart';
import 'package:bifrost_minting/algebras/staking_algebra.dart';
import 'package:async/async.dart';
import 'package:bifrost_minting/models/vrf_hit.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';

class BlockProducer extends BlockProducerAlgebra {
  final Stream<SlotData> parentHeaders;
  final StakingAlgebra staker;
  final ClockAlgebra clock;
  final BlockPackerAlgebra blockPacker;

  BlockProducer(this.parentHeaders, this.staker, this.clock, this.blockPacker);

  @override
  Future<Stream<FullBlock>> get blocks async {
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
    final completer = CancelableCompleter<FullBlock?>();

    unawaited(_nextEligibility(parentSlotData.slotId).then(
      (nextHit) async {
        if (nextHit != null)
          return _prepareBlockBody(
            parentSlotData,
            nextHit.slot,
            () => completer.isCanceled,
          )
              .then((bodyOpt) async {
                if (bodyOpt != null) {
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
                  if (maybeHeader != null)
                    return FullBlock(header: maybeHeader, fullBody: bodyOpt);
                }
              })
              .then((value) => completer.complete(value))
              .onError(completer.completeError);
        else
          return completer.complete(null);
      },
    ));

    return completer.operation;
  }

  Future<VrfHit?> _nextEligibility(SlotId parentSlotId) async {
    var test = clock.globalSlot;
    if (test < parentSlotId.slot) test = parentSlotId.slot;
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

  _prepareUnsignedBlock(SlotData parentSlotData, FullBlockBody fullBody,
          Int64 timestamp, VrfHit nextHit) =>
      (PartialOperationalCertificate partialOperationalCertificate) {
        final unsignedHeader = UnsignedBlockHeader(
            parentHeaderId: parentSlotData.slotId.blockId,
            parentSlot: parentSlotData.slotId.slot,
            txRoot: [], // TODO
            bloomFilter: [], // TODO
            timestamp: timestamp,
            height: parentSlotData.height + 1,
            slot: nextHit.slot,
            eligibilityCertificate: nextHit.eligibilityCertificate,
            partialOperationalCertificate: partialOperationalCertificate,
            metadata: [],
            address: staker.address);
        final body =
            BlockBody(transactionIds: fullBody.transaction.map((t) => t.id));
        return UnsignedBlock(unsignedHeader: unsignedHeader, body: body);
      };
}
