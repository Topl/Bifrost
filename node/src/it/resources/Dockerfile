FROM ubuntu:20.04

ARG GRAALVM_VERSION=21.1.0
ARG JAVA_VERSION=java11
ARG SBT_VERSION=1.5.0

RUN apt -y update && apt -y upgrade
RUN apt -y install \
            apt-transport-https \
            ca-certificates \
            curl \
            gnupg \
            lsb-release

# Docker install
# See: https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg && \
    echo \
      "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
      $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null && \
      apt -y update && DEBIAN_FRONTEND=noninteractive apt -y install docker-ce docker-ce-cli containerd.io

# GraalVM install
# See: https://github.com/graalvm/container/blob/master/community/Dockerfile
ARG GRAALVM_PKG=https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-$GRAALVM_VERSION/graalvm-ce-$JAVA_VERSION-linux-amd64-$GRAALVM_VERSION.tar.gz

ENV LANG=en_US.UTF-8 \
    JAVA_HOME=/opt/graalvm-ce-$JAVA_VERSION-$GRAALVM_VERSION/ \
    PATH=/opt/graalvm-ce-$JAVA_VERSION-$GRAALVM_VERSION/bin:$PATH

RUN set -eux \
    && curl --fail --silent --location --retry 3 ${GRAALVM_PKG} \
    | gunzip | tar x -C /opt/

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
