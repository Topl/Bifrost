import 'dart:async';

import 'package:bifrost_common/algebras/clock_algebra.dart';
import 'package:fpdart/src/tuple.dart';
import 'package:fixnum/fixnum.dart';

class Clock extends ClockAlgebra {
  final Duration _slotLength;
  final Int64 _slotsPerEpoch;
  final Int64 _genesisTimestamp;
  final Int64 _forwardBiasedSlotWindow;

  Clock(this._slotLength, this._slotsPerEpoch, this._genesisTimestamp,
      this._forwardBiasedSlotWindow);

  @override
  Timer delayedUntilSlot(Int64 slot) =>
      delayedUntilTimestamp(slotToTimestamps(slot).first);

  @override
  Timer delayedUntilTimestamp(Int64 timestamp) => Timer(
      Duration(milliseconds: (timestamp - localTimestamp).toInt()), () {});

  @override
  Int64 get forwardBiasedSlotWindow => _forwardBiasedSlotWindow;

  @override
  Int64 get globalEpoch => globalSlot ~/ slotsPerEpoch;

  @override
  Int64 get globalSlot => timestampToSlot(localTimestamp);

  @override
  Int64 get localTimestamp => Int64(DateTime.now().millisecondsSinceEpoch);

  @override
  Duration get slotLength => _slotLength;

  @override
  Tuple2<Int64, Int64> slotToTimestamps(Int64 slot) {
    final first = _genesisTimestamp + (slot * slotLength.inMilliseconds);
    final second = first + (slotLength.inMilliseconds - 1);
    return Tuple2(first, second);
  }

  @override
  Int64 get slotsPerEpoch => _slotsPerEpoch;

  @override
  Int64 timestampToSlot(Int64 timestamp) =>
      ((timestamp - _genesisTimestamp) ~/ _slotLength.inMilliseconds);
}
