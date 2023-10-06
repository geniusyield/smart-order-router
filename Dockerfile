FROM haskell:9.2.8-slim as builder

ENV LANG C.UTF-8

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        autoconf \
        automake \
        build-essential \
        chrony \
        libncursesw5 \
        liblzma-dev \
        libpq-dev \
        libssl-dev \
        libsystemd-dev \
        libtool \
        pkg-config \
        procps \
        tmux && \
    rm -rf /var/lib/apt/lists/*

# Libsodium:
RUN git clone https://github.com/input-output-hk/libsodium && \
    cd libsodium && \
    git checkout dbb48cc && \
    ./autogen.sh && \
    ./configure && \
    make && \
    make install

# Libsecp256k1:
RUN git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git checkout ac83be33d0956faf6b7f61a60ab524ef7d6a473a && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make install

ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

# ==================================[ BUILD ]========================================
WORKDIR /SOR

COPY . .

RUN cabal update
RUN cabal build all
RUN cabal test
RUN cabal install --global

# =============================[ SMART ORDER ROUTER ]================================

# Default values:
ENV BOTC_FP_NFT_POLICY='compiled-scripts/minting-policy'
ENV BOTC_FP_ORDER_VALIDATOR='compiled-scripts/partial-order'
ENV BOTC_EXECUTION_STRAT='OneSellToManyBuy'
ENV BOTC_RESCAN_DELAY='30000000'
ENV BOTC_MAX_ORDERS_MATCHES='5'
ENV BOTC_MAX_TXS_PER_ITERATION='4'
ENV BOTC_RANDOMIZE_MATCHES_FOUND='True'

ENTRYPOINT ["/bin/bash", "./start.sh"]
