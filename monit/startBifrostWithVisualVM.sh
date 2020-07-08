#!/bin/bash

PORT=1089
HOSTNAME=127.0.0.1
SSL=false
AUTH=false
LOCAL=false

cd ..
sbt -J-Dcom.sun.management.jmxremote \
    -J-Dcom.sun.management.jmxremote.port=$PORT \
    -J-Djava.rmi.server.hostname=$HOSTNAME \
    -J-Dcom.sun.management.jmxremote.ssl=$SSL \
    -J-Dcom.sun.management.jmxremote.authenticate=$AUTH \
    -J-Dcom.sun.management.jmxremote.local.only=$LOCAL \
    "runMain bifrost.BifrostApp"