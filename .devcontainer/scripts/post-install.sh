#!/bin/sh

# Minikube defaults to relatively low hardware settings
minikube config set memory 6000
minikube config set cpus 4
minikube config set disk-size 15gb
minikube start
minikube addons enable registry
minikube addons enable volumesnapshots
minikube addons enable csi-hostpath-driver
# Run a "background" container that pipes port 5000 from the k8s cluster to the host, allowing us to push Docker containers to the minikube registry
docker run --restart unless-stopped -d -i --network=host alpine ash -c "apk add socat && socat TCP-LISTEN:5000,reuseaddr,fork TCP:$(minikube ip):5000"
