FROM ubuntu:22.04

ARG SBT_VERSION=1.7.1

RUN apt -y update && apt -y upgrade
RUN apt -y install \
            apt-transport-https \
            ca-certificates \
            curl \
            gnupg \
            lsb-release \
            wget

# Docker install
# See: https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg && \
    echo \
      "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
      $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null && \
      apt -y update && DEBIAN_FRONTEND=noninteractive apt -y install docker-ce docker-ce-cli containerd.io

# JDK Install
RUN wget -O - https://packages.adoptium.net/artifactory/api/gpg/key/public | apt-key add - \
    && echo "deb https://packages.adoptium.net/artifactory/deb $(awk -F= '/^VERSION_CODENAME/{print$2}' /etc/os-release) main" | tee /etc/apt/sources.list.d/adoptium.list \
    && apt update \
    && apt install temurin-11-jdk

# SBT Install
ARG SBT_PKG=https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz
ENV PATH=/opt/sbt/bin:$PATH

RUN set -eux \
    && curl --fail --silent --location --retry 3 ${SBT_PKG} \
    | gunzip | tar x -C /opt/

WORKDIR /bifrost

COPY . /bifrost

RUN sbt compile

ENTRYPOINT sbt node/docker:publishLocal node/it:test
