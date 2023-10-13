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

RUN cabal update
COPY cabal.project ./
COPY geniusyield-orderbot.cabal ./
COPY geniusyield-dex-api/geniusyield-dex-api.cabal ./geniusyield-dex-api/
COPY geniusyield-orderbot-framework/geniusyield-orderbot-framework.cabal geniusyield-orderbot-framework/

RUN cabal update
RUN cabal build geniusyield-dex-api --only-dependencies

COPY . .

RUN cabal build all
RUN cabal test
RUN cabal install --global

# =============================[ SMART ORDER ROUTER ]================================
LABEL org.opencontainers.image.source="https://github.com/geniusyield/smart-order-router"

# Default values:
ENV BOTC_FP_NFT_POLICY='compiled-scripts/minting-policy'
ENV BOTC_FP_ORDER_VALIDATOR='compiled-scripts/partial-order'
ENV BOTC_EXECUTION_STRAT='OneSellToManyBuy'
ENV BOTC_RESCAN_DELAY='30000000'
ENV BOTC_MAX_ORDERS_MATCHES='5'
ENV BOTC_MAX_TXS_PER_ITERATION='4'
ENV BOTC_RANDOMIZE_MATCHES_FOUND='True'
ENV BOTC_ASSET_FILTER='[{"commodityAsset":"c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53","currencyAsset":"lovelace"},{"commodityAsset":"0254a6ffa78edb03ea8933dbd4ca078758dbfc0fc6bb0d28b7a9c89f.4c454e4649","currencyAsset":"lovelace"},{"commodityAsset":"8cafc9b387c9f6519cacdce48a8448c062670c810d8da4b232e56313.6d4e5458","currencyAsset":"lovelace"},{"commodityAsset":"171163f05e4f30b6be3c22668c37978e7d508b84f83558e523133cdf.74454d50","currencyAsset":"lovelace"}]'
ENV BOTC_POREFS='{"refAddr":"addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt","refNftAC":"fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.e6a295bb83d06f53fcf91151f54acec0a63fbd6f0d924206d5d012e6da3b72af","refNftUtxoRef":"39f987a6beb9cc4c45bba149a21c28068f640f3593f15f8157f0b6022b431977#0","scriptRef":"39f987a6beb9cc4c45bba149a21c28068f640f3593f15f8157f0b6022b431977#1","nftPolicyRef":"39f987a6beb9cc4c45bba149a21c28068f640f3593f15f8157f0b6022b431977#0"}'

ENTRYPOINT ["/bin/bash", "./start.sh"]
