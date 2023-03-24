FROM ubuntu:22.10

RUN apt-get update \
    && apt-get install -y wget apt-transport-https gpg git \
    && wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | gpg --dearmor -o /usr/share/keyrings/dart.gpg \
    && echo 'deb [signed-by=/usr/share/keyrings/dart.gpg arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main' | tee /etc/apt/sources.list.d/dart_stable.list \
    && apt-get update \
    && apt-get install -y dart protobuf-compiler \
    && echo 'export PATH="$PATH:/usr/lib/dart/bin"' >> ~/.profile \
    && echo 'export PATH="$PATH:$HOME/.pub-cache/bin"' >> ~/.profile \
    && export PATH="$PATH:/usr/lib/dart/bin" \
    && export PATH="$PATH:$HOME/.pub-cache/bin" \
    && dart pub global activate protoc_plugin \
    && dart pub global activate rps
