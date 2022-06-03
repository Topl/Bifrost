import 'package:flutter/material.dart';
import 'package:admin/src/proto/protobuf/topl_grpc.pbgrpc.dart';
import 'package:grpc/grpc.dart';
import 'package:grpc/grpc_connection_interface.dart';
import 'package:fast_base58/fast_base58.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final channel = ClientChannel(
      "localhost",
      port: 8090,
      options: const ChannelOptions(credentials: ChannelCredentials.insecure()),
    );
    return MaterialApp(
      title: 'Flutter Demo',
      theme: ThemeData(
        primarySwatch: Colors.green,
      ),
      home: MyHomePage(
          title: 'Flutter Demo Home Page', client: ToplGrpcClient(channel)),
    );
  }
}

class MyHomePage extends StatelessWidget {
  const MyHomePage({Key? key, required this.title, required this.client})
      : super(key: key);

  final String title;
  final ToplGrpcClient client;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            StreamBuilder<BlockAdoptionsRes>(
              stream: client.blockAdoptions(BlockAdoptionsReq()),
              builder: (context, snapshot) => Text(
                snapshot.hasData
                    ? Base58Encode(snapshot.data!.blockId)
                    : "No Data",
              ),
            )
          ],
        ),
      ),
    );
  }
}
