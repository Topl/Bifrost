#!/usr/bin/env bash

cd ..

sbt assembly

cp target/scala-2.12/bifrost-assembly-3.0.0.jar docker/

cd docker || exit

docker build -t toplprotocol/bifrost:3.0.0 .