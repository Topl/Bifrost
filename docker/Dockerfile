FROM java:8-jre

RUN mkdir -p /usr/src/myapp

COPY ./bifrost-assembly-1.1.0.jar /usr/src/myapp

WORKDIR /usr/src/myapp

CMD ["java", "-jar", "bifrost-1.1.0.jar"]
