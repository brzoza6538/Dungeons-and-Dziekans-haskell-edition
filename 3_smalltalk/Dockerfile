FROM ubuntu:20.04

RUN apt-get update && apt-get install -y \
    gnu-smalltalk \
    && apt-get clean

WORKDIR /app

ENTRYPOINT ["gst"]
