#!/bin/sh

docker tag toplprotocol/network-delayer localhost:5000/topl/network-delayer && docker push localhost:5000/topl/network-delayer
docker tag toplprotocol/bifrost-node localhost:5000/topl/bifrost-node && docker push localhost:5000/topl/bifrost-node
docker tag toplprotocol/testnet-simulation-orchestrator localhost:5000/topl/testnet-simulation-orchestrator && docker push localhost:5000/topl/testnet-simulation-orchestrator