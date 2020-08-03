FROM oracle/graalvm-ce:19.3.0

RUN mkdir -p /usr/src/myapp

COPY ./bifrost-assembly-3.0.0.jar /usr/src/myapp

WORKDIR /usr/src/myapp

CMD ["java", "-jar", "bifrost-assembly-1.0.0.jar", "testnet-private.json"]