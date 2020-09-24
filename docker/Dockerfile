FROM oracle/graalvm-ce:19.2.1

RUN mkdir -p /usr/src/bifrost

COPY ./bifrost-1.1.0.jar /usr/src/bifrost

WORKDIR /usr/src/bifrost

VOLUME ["/usr/src/bifrost/.bifrost"]
CMD ["java", "-jar", "bifrost-1.1.0.jar"]