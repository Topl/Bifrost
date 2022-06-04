///
//  Generated code. Do not modify.
//  source: protobuf/topl_grpc.proto
//
// @dart = 2.12
// ignore_for_file: annotate_overrides,camel_case_types,unnecessary_const,non_constant_identifier_names,library_prefixes,unused_import,unused_shown_name,return_of_invalid_type,unnecessary_this,prefer_final_fields,deprecated_member_use_from_same_package

import 'dart:core' as $core;
import 'dart:convert' as $convert;
import 'dart:typed_data' as $typed_data;
@$core.Deprecated('Use broadcastTxReqDescriptor instead')
const BroadcastTxReq$json = const {
  '1': 'BroadcastTxReq',
  '2': const [
    const {'1': 'transmittableBytes', '3': 1, '4': 1, '5': 12, '10': 'transmittableBytes'},
  ],
};

/// Descriptor for `BroadcastTxReq`. Decode as a `google.protobuf.DescriptorProto`.
final $typed_data.Uint8List broadcastTxReqDescriptor = $convert.base64Decode('Cg5Ccm9hZGNhc3RUeFJlcRIuChJ0cmFuc21pdHRhYmxlQnl0ZXMYASABKAxSEnRyYW5zbWl0dGFibGVCeXRlcw==');
@$core.Deprecated('Use broadcastTxResDescriptor instead')
const BroadcastTxRes$json = const {
  '1': 'BroadcastTxRes',
};

/// Descriptor for `BroadcastTxRes`. Decode as a `google.protobuf.DescriptorProto`.
final $typed_data.Uint8List broadcastTxResDescriptor = $convert.base64Decode('Cg5Ccm9hZGNhc3RUeFJlcw==');
@$core.Deprecated('Use blockAdoptionsReqDescriptor instead')
const BlockAdoptionsReq$json = const {
  '1': 'BlockAdoptionsReq',
};

/// Descriptor for `BlockAdoptionsReq`. Decode as a `google.protobuf.DescriptorProto`.
final $typed_data.Uint8List blockAdoptionsReqDescriptor = $convert.base64Decode('ChFCbG9ja0Fkb3B0aW9uc1JlcQ==');
@$core.Deprecated('Use blockAdoptionsResDescriptor instead')
const BlockAdoptionsRes$json = const {
  '1': 'BlockAdoptionsRes',
  '2': const [
    const {'1': 'blockId', '3': 1, '4': 1, '5': 12, '10': 'blockId'},
  ],
};

/// Descriptor for `BlockAdoptionsRes`. Decode as a `google.protobuf.DescriptorProto`.
final $typed_data.Uint8List blockAdoptionsResDescriptor = $convert.base64Decode('ChFCbG9ja0Fkb3B0aW9uc1JlcxIYCgdibG9ja0lkGAEgASgMUgdibG9ja0lk');
@$core.Deprecated('Use fetchBlockHeaderReqDescriptor instead')
const FetchBlockHeaderReq$json = const {
  '1': 'FetchBlockHeaderReq',
  '2': const [
    const {'1': 'blockId', '3': 1, '4': 1, '5': 12, '10': 'blockId'},
  ],
};

/// Descriptor for `FetchBlockHeaderReq`. Decode as a `google.protobuf.DescriptorProto`.
final $typed_data.Uint8List fetchBlockHeaderReqDescriptor = $convert.base64Decode('ChNGZXRjaEJsb2NrSGVhZGVyUmVxEhgKB2Jsb2NrSWQYASABKAxSB2Jsb2NrSWQ=');
@$core.Deprecated('Use fetchBlockHeaderResDescriptor instead')
const FetchBlockHeaderRes$json = const {
  '1': 'FetchBlockHeaderRes',
  '2': const [
    const {'1': 'header', '3': 1, '4': 1, '5': 11, '6': '.co.topl.grpc.services.BlockHeader', '10': 'header'},
  ],
};

/// Descriptor for `FetchBlockHeaderRes`. Decode as a `google.protobuf.DescriptorProto`.
final $typed_data.Uint8List fetchBlockHeaderResDescriptor = $convert.base64Decode('ChNGZXRjaEJsb2NrSGVhZGVyUmVzEjoKBmhlYWRlchgBIAEoCzIiLmNvLnRvcGwuZ3JwYy5zZXJ2aWNlcy5CbG9ja0hlYWRlclIGaGVhZGVy');
@$core.Deprecated('Use blockHeaderDescriptor instead')
const BlockHeader$json = const {
  '1': 'BlockHeader',
  '2': const [
    const {'1': 'parentHeaderIdBytes', '3': 1, '4': 1, '5': 12, '10': 'parentHeaderIdBytes'},
    const {'1': 'parentSlot', '3': 2, '4': 1, '5': 3, '10': 'parentSlot'},
    const {'1': 'txRootBytes', '3': 3, '4': 1, '5': 12, '10': 'txRootBytes'},
    const {'1': 'bloomFilterBytes', '3': 4, '4': 1, '5': 12, '10': 'bloomFilterBytes'},
    const {'1': 'timestamp', '3': 5, '4': 1, '5': 3, '10': 'timestamp'},
    const {'1': 'height', '3': 6, '4': 1, '5': 3, '10': 'height'},
    const {'1': 'slot', '3': 7, '4': 1, '5': 3, '10': 'slot'},
    const {'1': 'eligibilityCertificateBytes', '3': 8, '4': 1, '5': 12, '10': 'eligibilityCertificateBytes'},
    const {'1': 'operationalCertificateBytes', '3': 9, '4': 1, '5': 12, '10': 'operationalCertificateBytes'},
    const {'1': 'metadataBytes', '3': 10, '4': 1, '5': 12, '10': 'metadataBytes'},
    const {'1': 'addressBytes', '3': 11, '4': 1, '5': 12, '10': 'addressBytes'},
  ],
};

/// Descriptor for `BlockHeader`. Decode as a `google.protobuf.DescriptorProto`.
final $typed_data.Uint8List blockHeaderDescriptor = $convert.base64Decode('CgtCbG9ja0hlYWRlchIwChNwYXJlbnRIZWFkZXJJZEJ5dGVzGAEgASgMUhNwYXJlbnRIZWFkZXJJZEJ5dGVzEh4KCnBhcmVudFNsb3QYAiABKANSCnBhcmVudFNsb3QSIAoLdHhSb290Qnl0ZXMYAyABKAxSC3R4Um9vdEJ5dGVzEioKEGJsb29tRmlsdGVyQnl0ZXMYBCABKAxSEGJsb29tRmlsdGVyQnl0ZXMSHAoJdGltZXN0YW1wGAUgASgDUgl0aW1lc3RhbXASFgoGaGVpZ2h0GAYgASgDUgZoZWlnaHQSEgoEc2xvdBgHIAEoA1IEc2xvdBJAChtlbGlnaWJpbGl0eUNlcnRpZmljYXRlQnl0ZXMYCCABKAxSG2VsaWdpYmlsaXR5Q2VydGlmaWNhdGVCeXRlcxJAChtvcGVyYXRpb25hbENlcnRpZmljYXRlQnl0ZXMYCSABKAxSG29wZXJhdGlvbmFsQ2VydGlmaWNhdGVCeXRlcxIkCg1tZXRhZGF0YUJ5dGVzGAogASgMUg1tZXRhZGF0YUJ5dGVzEiIKDGFkZHJlc3NCeXRlcxgLIAEoDFIMYWRkcmVzc0J5dGVz');
