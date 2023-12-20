import 'package:admin/constants.dart';
import 'package:admin/rpc_ops.dart';
import 'package:admin/state.dart';
import 'package:admin/ui/blockchain_state_viewer.dart';
import 'package:flutter/material.dart';
import 'package:topl_common/genus/services/node_grpc.dart';

class BifrostAdminHomePage extends StatefulWidget {
  const BifrostAdminHomePage({super.key, required this.title});

  final String title;

  @override
  State<BifrostAdminHomePage> createState() => _BifrostAdminHomePageState();
}

class _BifrostAdminHomePageState extends State<BifrostAdminHomePage> {
  String currentAddress = "";
  String _addressBuffer = "$rpcHost:$rpcPort";

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text(widget.title)),
      body: currentAddress.isEmpty ? _waitingForInput : _ready,
    );
  }

  Widget get _waitingForInput => Center(child: _addressBar);

  Widget get _ready {
    final split = currentAddress.split(":");
    final client = NodeGRPCService(host: split[0], port: int.parse(split[1]));
    return SingleChildScrollView(
      child: Column(
        children: [
          _addressBar,
          StreamBuilder(
            stream: Stream.fromFuture(client.whenReady()).asyncExpand(
                (_) => BlockchainState.streamed(client, maxCacheSize, 100)),
            builder: (context, snapshot) => snapshot.hasData
                ? BlockchainStateViewer(state: snapshot.data!)
                : const CircularProgressIndicator(),
          ),
        ],
      ),
    );
  }

  Widget get _addressBar => SizedBox(
        height: 100,
        child: Row(
          children: [
            SizedBox(
              width: 300,
              child: TextFormField(
                decoration: const InputDecoration(
                    hintText: "host:port", border: OutlineInputBorder()),
                initialValue: _addressBuffer,
                onChanged: (text) => _addressBuffer = text,
              ),
            ),
            IconButton(
              onPressed: () => setState(() {
                currentAddress = _addressBuffer;
              }),
              icon: const Icon(Icons.send),
            )
          ],
        ),
      );
}
