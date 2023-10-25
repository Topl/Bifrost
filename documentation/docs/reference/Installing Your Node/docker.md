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
mkdir ./bifrost-data
sudo chown  1001 ./bifrost-data
docker run -d --name bifrost-tetra $(pwd)/bifrost-data:/tmp/bifrost -p 9084:9084 -p 9085:9085 toplprotocol/bifrost-node:2.0.0-alpha2
docker ps
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
docker run -it --rm -p 9084:9084 -p 9085:9085 toplprotocol/bifrost-node:2.0.0-alpha2
```

Run in background

```sh
docker run -d --name bifrost-tetra -p 9084:9084 -p 9085:9085 toplprotocol/bifrost-node:2.0.0-alpha2
```

Run with volume to persist data

```sh
mkdir ./bifrost-data
sudo chown  1001 ./bifrost-data
docker run -d --name bifrost-tetra $(pwd)/bifrost-data:/tmp/bifrost -p 9084:9084 -p 9085:9085 toplprotocol/bifrost-node:2.0.0-alpha2
```

Interact with the node

You can use gRPC to query the node at http://localhost:9084.
