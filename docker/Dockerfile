FROM ghcr.io/graalvm/graalvm-ce:java8-21.0.0

RUN mkdir -p /usr/src/bifrost

COPY ./bifrost-1.1.0.jar /usr/src/bifrost

WORKDIR /usr/src/bifrost

VOLUME ["/usr/src/bifrost/.bifrost"]
CMD ["java", "-jar", "bifrost-1.1.0.jar"]