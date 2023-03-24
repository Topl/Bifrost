import 'package:rational/rational.dart';
import 'package:topl_protobuf/consensus/models/eligibility_certificate.pb.dart';

import 'package:fixnum/fixnum.dart';

class VrfHit {
  final EligibilityCertificate cert;
  final Int64 slot;
  final Rational threshold;

  VrfHit(this.cert, this.slot, this.threshold);
}
