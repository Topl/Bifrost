FROM ubuntu:20.10

ARG GRAALVM_VERSION=21.0.0.2
ARG JAVA_VERSION=java8
ARG SBT_VERSION=1.4.9

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
      apt -y update && apt -y install docker-ce docker-ce-cli containerd.io

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
# See: https://github.com/mozilla/docker-sbt/blob/main/Dockerfile
RUN \
  mkdir /working/ && \
  cd /working/ && \
  curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update && \
  apt-get install sbt && \
  cd && \
  rm -r /working/ && \
  sbt sbtVersion

WORKDIR /bifrost

COPY . /bifrost

RUN sbt compile

ENTRYPOINT sbt node/docker:publishLocal node/it:test
