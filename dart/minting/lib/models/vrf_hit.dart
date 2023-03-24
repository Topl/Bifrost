import 'package:topl_protobuf/consensus/models/eligibility_certificate.pb.dart';

import 'package:fixnum/fixnum.dart';
import 'package:bifrost_common/models/ratio.dart';

class VrfHit {
  final EligibilityCertificate cert;
  final Int64 slot;
  final Ratio threshold;

  VrfHit(this.cert, this.slot, this.threshold);
}
