FROM ubuntu:14.10

RUN apt-get update && apt-get install -y software-properties-common
RUN add-apt-repository -y ppa:hvr/ghc
RUN apt-get update && apt-get install -y \
    cabal-install-1.20 \
    ghc-7.8.4 \
    git \
    wget \
    zlib1g-dev

ENV LANG C.UTF-8
ENV PORT 80
ENV PATH /opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:/.cabal/bin:$PATH
ENV HOME /home

RUN cabal update
RUN cabal install -j4 alex happy

RUN git clone https://github.com/yanhick/conchord-ui.git /conchord

WORKDIR /conchord
RUN cabal install -j4 --only-dependencies
RUN cabal configure
RUN cabal build -j4

CMD ./dist/build/conchord-ui
