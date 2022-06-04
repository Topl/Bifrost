///
//  Generated code. Do not modify.
//  source: protobuf/topl_grpc.proto
//
// @dart = 2.12
// ignore_for_file: annotate_overrides,camel_case_types,unnecessary_const,non_constant_identifier_names,library_prefixes,unused_import,unused_shown_name,return_of_invalid_type,unnecessary_this,prefer_final_fields

import 'dart:async' as $async;

import 'dart:core' as $core;

import 'package:grpc/service_api.dart' as $grpc;
import 'topl_grpc.pb.dart' as $0;
export 'topl_grpc.pb.dart';

class ToplGrpcClient extends $grpc.Client {
  static final _$broadcastTx =
      $grpc.ClientMethod<$0.BroadcastTxReq, $0.BroadcastTxRes>(
          '/co.topl.grpc.services.ToplGrpc/BroadcastTx',
          ($0.BroadcastTxReq value) => value.writeToBuffer(),
          ($core.List<$core.int> value) => $0.BroadcastTxRes.fromBuffer(value));
  static final _$blockAdoptions =
      $grpc.ClientMethod<$0.BlockAdoptionsReq, $0.BlockAdoptionsRes>(
          '/co.topl.grpc.services.ToplGrpc/BlockAdoptions',
          ($0.BlockAdoptionsReq value) => value.writeToBuffer(),
          ($core.List<$core.int> value) =>
              $0.BlockAdoptionsRes.fromBuffer(value));
  static final _$fetchBlockHeader =
      $grpc.ClientMethod<$0.FetchBlockHeaderReq, $0.FetchBlockHeaderRes>(
          '/co.topl.grpc.services.ToplGrpc/FetchBlockHeader',
          ($0.FetchBlockHeaderReq value) => value.writeToBuffer(),
          ($core.List<$core.int> value) =>
              $0.FetchBlockHeaderRes.fromBuffer(value));

  ToplGrpcClient($grpc.ClientChannel channel,
      {$grpc.CallOptions? options,
      $core.Iterable<$grpc.ClientInterceptor>? interceptors})
      : super(channel, options: options, interceptors: interceptors);

  $grpc.ResponseFuture<$0.BroadcastTxRes> broadcastTx($0.BroadcastTxReq request,
      {$grpc.CallOptions? options}) {
    return $createUnaryCall(_$broadcastTx, request, options: options);
  }

  $grpc.ResponseStream<$0.BlockAdoptionsRes> blockAdoptions(
      $0.BlockAdoptionsReq request,
      {$grpc.CallOptions? options}) {
    return $createStreamingCall(
        _$blockAdoptions, $async.Stream.fromIterable([request]),
        options: options);
  }

  $grpc.ResponseFuture<$0.FetchBlockHeaderRes> fetchBlockHeader(
      $0.FetchBlockHeaderReq request,
      {$grpc.CallOptions? options}) {
    return $createUnaryCall(_$fetchBlockHeader, request, options: options);
  }
}

abstract class ToplGrpcServiceBase extends $grpc.Service {
  $core.String get $name => 'co.topl.grpc.services.ToplGrpc';

  ToplGrpcServiceBase() {
    $addMethod($grpc.ServiceMethod<$0.BroadcastTxReq, $0.BroadcastTxRes>(
        'BroadcastTx',
        broadcastTx_Pre,
        false,
        false,
        ($core.List<$core.int> value) => $0.BroadcastTxReq.fromBuffer(value),
        ($0.BroadcastTxRes value) => value.writeToBuffer()));
    $addMethod($grpc.ServiceMethod<$0.BlockAdoptionsReq, $0.BlockAdoptionsRes>(
        'BlockAdoptions',
        blockAdoptions_Pre,
        false,
        true,
        ($core.List<$core.int> value) => $0.BlockAdoptionsReq.fromBuffer(value),
        ($0.BlockAdoptionsRes value) => value.writeToBuffer()));
    $addMethod(
        $grpc.ServiceMethod<$0.FetchBlockHeaderReq, $0.FetchBlockHeaderRes>(
            'FetchBlockHeader',
            fetchBlockHeader_Pre,
            false,
            false,
            ($core.List<$core.int> value) =>
                $0.FetchBlockHeaderReq.fromBuffer(value),
            ($0.FetchBlockHeaderRes value) => value.writeToBuffer()));
  }

  $async.Future<$0.BroadcastTxRes> broadcastTx_Pre(
      $grpc.ServiceCall call, $async.Future<$0.BroadcastTxReq> request) async {
    return broadcastTx(call, await request);
  }

  $async.Stream<$0.BlockAdoptionsRes> blockAdoptions_Pre($grpc.ServiceCall call,
      $async.Future<$0.BlockAdoptionsReq> request) async* {
    yield* blockAdoptions(call, await request);
  }

  $async.Future<$0.FetchBlockHeaderRes> fetchBlockHeader_Pre(
      $grpc.ServiceCall call,
      $async.Future<$0.FetchBlockHeaderReq> request) async {
    return fetchBlockHeader(call, await request);
  }

  $async.Future<$0.BroadcastTxRes> broadcastTx(
      $grpc.ServiceCall call, $0.BroadcastTxReq request);
  $async.Stream<$0.BlockAdoptionsRes> blockAdoptions(
      $grpc.ServiceCall call, $0.BlockAdoptionsReq request);
  $async.Future<$0.FetchBlockHeaderRes> fetchBlockHeader(
      $grpc.ServiceCall call, $0.FetchBlockHeaderReq request);
}
