---
id: install-bare-metal
title: Run on Bare Metal
tags:
  - Install
  - Getting started
  - sdkman
  - scala
  - jar
---

## Ubuntu

### Install SDKMAN and Java

```sh
sudo apt install curl zip unzip
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk install java
```

### Download and Run Jar

Locate the latest version from https://github.com/Topl/Project-Bifrost/releases/latest 

ex:

```sh
wget https://github.com/Topl/Bifrost/releases/download/v2.0.0-alpha2/bifrost-node-2.0.0-alpha2.jar 
```

Run

```sh
java -jar bifrost-node-2.0.0-alpha2.jar
```

To see other options, use --help.

```sh
--config <str>              Zero or more config files (.conf, .json, .yaml) to apply to the node.
                            Config files stack such that the last config file takes precedence. To
                            specify an internal resource, prefix the value with "resource://".
--dataDir <str>             The directory to use when saving/reading blockchain data
--debug                     An optional flag to enable debug mode on this node.
--disableGenus              Disables the Genus server and Genus gRPC services
--knownPeers <str>          A comma-delimited list of host:port values to connect to at launch
                            (i.e. 1.2.3.4:9084,5.6.7.8:9084)
--logbackFile <str>         An optional path to a logback.xml file to override the logging
                            configuration of the node.
--orientDbDir <str>         The directory to use when saving/reading graph data
--orientDbPassword <str>    The password to use when connecting to OrientDB
--p2pBindHost <str>         The hostname to bind to for the P2P layer (i.e. localhost or 0.0.0.0)
--p2pBindPort <int>         The port to bind to for the P2P layer (i.e. 9084)
--rpcBindHost <str>         The hostname to bind to for the RPC layer (i.e. localhost or 0.0.0.0)
--rpcBindPort <int>         The port to bind to for the RPC layer (i.e. 9085)
--stakingDir <str>          The directory of the block producer's staking keys
--testnetStakerCount <int>  The number of stakers to initialize.
--testnetStakerIndex <int>  The index of the staker to launch.
--testnetTimestamp <long>   A UTC Unix epoch timestamp (ms) to use when seeding a private testnet.
```
