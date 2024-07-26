---
id: connect-to-toplnet
title: Connect To Toplnet
tags:
  - toplnet
  - relay
  - Getting started
---

## gRPC Endpoint

https://toplnet.topl.co:443

## P2P

https://toplnet.topl.co:9085

## How to Connect

In this example, we will be using Docker to run the node.

The way to connect to `toplnet` is by adding it as a known peer. To do this via CLI, you can use `--knownPeers testnet.topl.co:9085`. You will also need to pass the `--config` which contains the genesis block information and other configuration requirements for the given testnet.

The testnet configs are hosted on GitHub https://github.com/Topl/Genesis which can be automatically downloaded by Bifrost using the `--config https://raw.githubusercontent.com/Topl/Genesis/main/toplnet/config.yaml` parameter.

A full example of connecting to the node:

```
mkdir .bifrost
sudo chown 1001 .bifrost -R -v; sudo chgrp 0 .bifrost -R -v

docker run --name bifrost-toplnet -d -v $(pwd)/.bifrost:/bifrost -p 9084:9084 -p 9085:9085 toplprotocol/bifrost-node:2.0.0-beta3 \
    --knownPeers toplnet.topl.co:9085 \
    --config https://raw.githubusercontent.com/Topl/Genesis/main/toplnet/config.yaml
```

Alternatively, you can use Docker-Compose to manage the volumes for you instead. This will include `gRPC-web` as a proxy for accessing Genus on port `9086`:

```
wget https://raw.githubusercontent.com/Topl/Genesis/docker-compose/toplnet/docker-compose.yaml && docker compose up -d --pull always && docker compose logs -f node
```

## How to Get Funds

Funds can be retrieved via our Faucet. https://faucet.topl.co/#/

Detailed instructions coming soon!

## Visualize Transactions

Transactions can be viewed on our block explorer. https://explore.topl.co/#/

![Annulus](../../../static/img/annulus.png)

Select the correct network from the dropdown in the top right corner and search using the search box.

Detailed instructions coming soon!
