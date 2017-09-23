FROM java:8-jre

RUN mkdir -p /usr/src/myapp

COPY ./project-bifrost-assembly-0.1.1-alpha.jar /usr/src/myapp

WORKDIR /usr/src/myapp

CMD ["java", "-jar", "project-bifrost-assembly-0.1.1-alpha.jar"]
