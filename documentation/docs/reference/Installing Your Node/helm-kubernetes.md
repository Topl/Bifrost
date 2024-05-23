---
id: install-k8s
title: Run with Helm/Kubernetes
tags:
  - Install
  - Getting started
  - sdkman
  - scala
  - jar
---

## Quickstart

### Tools needed:

- Docker
- Kubernetes
- Helm

```yaml values.yaml
args: ['--dataDir', '/mnt/bifrost/data', '--stakingDir', '/mnt/bifrost/staking']

volume:
  mountDirectory: /mnt/bifrost

helm repo add topl https://topl.github.io/helm-charts
helm repo update
helm upgrade --install bifrost topl/bifrost -n bifrost --create-namespace -f ./values.yaml
```

## Install K8s cluster

### Ubuntu

In this example, we will use microk8s. There are many others you can use, including Docker Desktop + K8s, Minikube, k3s, Kind, etc. 

https://microk8s.io/docs/getting-started 

```sh
sudo snap install microk8s --classic
# Add user to group
sudo usermod -a -G microk8s $USER
sudo chown -f -R $USER ~/.kube
```

#### Enable add ons

```sh
microk8s enable dns hostpath-storage
```

### Windows

Follow Docker Desktop installation instructions: ​ https://www.docker.com/products/docker-desktop/ 
I recommend using the WSL2 backend. Instructions are here: ​https://docs.docker.com/desktop/windows/wsl/ 

Once Docker Desktop is installed, k8s is as easy as clicking a button

Add an Alias

You may also want to create an alias so you don’t have to type microk8s kubectl every time. A common way would be do 

```sh
vim ~/.bash_aliases
```

Add this line

```sh
alias k="microk8s kubectl"
alias helm="microk8s helm"
```

Reload the shell, and then you can run commands like

```sh
k get pods
```

The rest of the guide assumes an alias. If you didn’t add one, use the kubectl , microk8s kubectl, minikube kubectl, etc.

Install Helm Chart

Add the Apparatus Helm repository

```sh
helm repo add topl https://topl.github.io/helm-charts
helm repo update
```

Install the Bifrost helm chart

```sh
helm upgrade --install bifrost topl/bifrost -n bifrost --create-namespace
```

You can pass values to the chart either by passing a values.yaml file, or individual values via the --set flag.

```sh
helm upgrade --install bifrost topl/bifrost -n bifrost --create-namespace -f ./path/to/values.yaml
helm upgrade --install bifrost topl/bifrost --set volume.mountDirectory='/mnt/bifrost' -n bifrost --create-namespace
```

Finally, check the pod to make sure it is running.

```sh
k get pods -n bifrost

# To get more detailed info if the pod did not start, run
k describe pods -n bifrost
```

You can view the logs by running:

```sh
k logs statefulset/bifrost -n bifrost

# Or individual pods:
k logs bifrost-0 -n bifrost
```
