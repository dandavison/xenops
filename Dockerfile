FROM ubuntu

RUN apt-get update && \
    apt-get install -y \
        software-properties-common

RUN add-apt-repository ppa:kelleyk/emacs && \
    apt-get update && \
    apt-get install -y \
        emacs26 \
        git \
        libgl1-mesa-glx \
        mesa-utils \
        texinfo \
        texlive-base \
        texlive-binaries \        
        texlive-latex-base \
        texlive-fonts-recommended \
        texlive-generic-recommended

VOLUME /host
ENV XENOPS_DOCKER 1
RUN mkdir /tmp/xenops-packages
COPY tests/setup/init.el tests/setup/install-deps.el /tmp/
RUN emacs --batch --load /tmp/init.el --load /tmp/install-deps.el && rm /tmp/*.el
COPY . /xenops
WORKDIR /xenops
