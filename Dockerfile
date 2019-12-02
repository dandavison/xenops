FROM ubuntu
RUN apt-get update && apt-get install -y emacs make
COPY . xenops
WORKDIR xenops
