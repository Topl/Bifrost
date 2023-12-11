---
id: install-docker
title: Run with Docker
tags:
  - Install
  - Getting started
  - docker
  - run
---

## Quickstart

```sh
mkdir .bifrost
sudo chown 1001 .bifrost -R -v; sudo chgrp 0 .bifrost -R -v

docker run --name bifrost -d -v $(pwd)/.bifrost:/bifrost -p 9085:9085 toplprotocol/bifrost-node:2.0.0-beta1 --knownPeers toplnet.topl.co:9085 --disableGenus --config https://raw.githubusercontent.com/Topl/Genesis/main/toplnet/config.yaml

docker ps
docker logs -f bifrost-tetra
```

## Ubuntu

Follow Docker’s install instructions: https://docs.docker.com/engine/install/ubuntu/ 

Also do the post install commands to add your user to the docker group to allow running docker commands without sudo. https://docs.docker.com/engine/install/linux-postinstall/ 

Restart or log out and back in.

## Windows

Follow Docker Desktop installation instructions: https://www.docker.com/products/docker-desktop/ 
I recommend using the WSL2 backend. Instructions are here: https://docs.docker.com/desktop/windows/wsl/ 

Run Bifrost Docker Image Directly

Find the latest Tetra image from https://hub.docker.com/r/toplprotocol/bifrost-node/tags 

Note: Latest is currently tagged to Dion, our previous version of Bifrost. To use Tetra, use the bifrost-node:2.x.x images.

Run in terminal

```sh
docker run -it --rm -p 9084:9084 -p 9085:9085 toplprotocol/bifrost-node:2.0.0-beta1
```

Run in background

```sh
docker run -d --name bifrost-tetra -p 9084:9084 -p 9085:9085 toplprotocol/bifrost-node:2.0.0-beta1
```

Run with volume to persist data

```sh
mkdir .bifrost
sudo chown  1001 .bifrost
docker run -d --name bifrost-tetra -v $(pwd)/.bifrost:/bifrost -p 9084:9084 -p 9085:9085 toplprotocol/bifrost-node:2.0.0-beta1
```

Interact with the node

You can use gRPC to query the node at http://localhost:9084.
