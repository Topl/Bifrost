import 'dart:typed_data';

import 'package:bifrost_common/models/common.dart';
import 'package:bifrost_common/utils.dart';

class VrfArgument {
  final Eta eta;
  final Slot slot;

  VrfArgument(this.eta, this.slot);

  Uint8List get signableBytes =>
      Uint8List.fromList(eta..addAll(slot.toBigInt.bytes));
}
