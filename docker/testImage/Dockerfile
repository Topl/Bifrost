FROM ghcr.io/graalvm/graalvm-ce:java8-21.0.0

ENV SCALA_VERSION 2.12.13
ENV SBT_VERSION 1.4.6

RUN microdnf update -y \
 && microdnf install -y wget git \
 && curl https://bintray.com/sbt/rpm/rpm | tee /etc/yum.repos.d/bintray-sbt-rpm.repo \
 && microdnf install -y sbt
