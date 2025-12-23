FROM ghcr.io/geniusyield/haskell-base-image:9.6.5

# ==================================[ BUILD ]========================================
WORKDIR /SOR

RUN cabal update
COPY cabal.project ./
COPY geniusyield-orderbot.cabal ./
COPY geniusyield-annset/geniusyield-annset.cabal ./geniusyield-annset/
COPY geniusyield-orderbot-framework/geniusyield-orderbot-framework.cabal geniusyield-orderbot-framework/

RUN cabal update

COPY . .

# Ensure we run a known Fourmolu release regardless of the base image version
RUN curl -L https://github.com/fourmolu/fourmolu/releases/download/v0.17.0.0/fourmolu-0.17.0.0-linux-x86_64 -o /usr/local/bin/fourmolu \
 && chmod +x /usr/local/bin/fourmolu

# Run FOURMOLU checks (check code formatting - but skip the dist-newstyle folder):
RUN fourmolu --mode check .

RUN cabal build all
RUN cabal test
RUN cabal install --global

# =============================[ SMART ORDER ROUTER ]================================
LABEL org.opencontainers.image.source="https://github.com/geniusyield/smart-order-router"

# Default values:
ENV BOTC_EXECUTION_STRAT='OneSellToManyBuy'
ENV BOTC_RESCAN_DELAY='30000000'
ENV BOTC_MAX_ORDERS_MATCHES='5'
ENV BOTC_MAX_TXS_PER_ITERATION='4'
ENV BOTC_RANDOMIZE_MATCHES_FOUND='True'
ENV BOTC_ASSET_FILTER='[{ "commodityAsset" : "1d7f33bd23d85e1a25d87d86fac4f199c3197a2f7afeb662a0f34e1e.776f726c646d6f62696c65746f6b656e", "currencyAsset" : "lovelace"},{ "commodityAsset" : "1ddcb9c9de95361565392c5bdff64767492d61a96166cb16094e54be.4f5054", "currencyAsset" : "lovelace"},{ "commodityAsset" : "25f0fc240e91bd95dcdaebd2ba7713fc5168ac77234a3d79449fc20c.534f4349455459", "currencyAsset" : "lovelace"},{ "commodityAsset" : "279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f.534e454b", "currencyAsset" : "lovelace"},{ "commodityAsset" : "29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6.4d494e", "currencyAsset" : "lovelace"},{ "commodityAsset" : "51a5e236c4de3af2b8020442e2a26f454fda3b04cb621c1294a0ef34.424f4f4b", "currencyAsset" : "lovelace"},{ "commodityAsset" : "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0.494e4459", "currencyAsset" : "lovelace"},{ "commodityAsset" : "5c1c91a65bedac56f245b8184b5820ced3d2f1540e521dc1060fa683.4a454c4c59", "currencyAsset" : "lovelace"},{ "commodityAsset" : "5d16cc1a177b5d9ba9cfa9793b07e60f1fb70fea1f8aef064415d114.494147", "currencyAsset" : "lovelace"},{ "commodityAsset" : "5dac8536653edc12f6f5e1045d8164b9f59998d3bdc300fc92843489.4e4d4b52", "currencyAsset" : "lovelace"},{ "commodityAsset" : "681b5d0383ac3b457e1bcc453223c90ccef26b234328f45fa10fd276.4a5047", "currencyAsset" : "lovelace"},{ "commodityAsset" : "682fe60c9918842b3323c43b5144bc3d52a23bd2fb81345560d73f63.4e45574d", "currencyAsset" : "lovelace"},{ "commodityAsset" : "6ac8ef33b510ec004fe11585f7c5a9f0c07f0c23428ab4f29c1d7d10.4d454c44", "currencyAsset" : "lovelace"},{ "commodityAsset" : "6c8642400e8437f737eb86df0fc8a8437c760f48592b1ba8f5767e81.456d706f7761", "currencyAsset" : "lovelace"},{ "commodityAsset" : "75fcc276057db5fc48eae0e11453c773c8a54604c3086bf9d95ac1b7.43485259", "currencyAsset" : "lovelace"},{ "commodityAsset" : "7914fae20eb2903ed6fd5021a415c1bd2626b64a2d86a304cb40ff5e.4c494649", "currencyAsset" : "lovelace"},{ "commodityAsset" : "804f5544c1962a40546827cab750a88404dc7108c0f588b72964754f.56594649", "currencyAsset" : "lovelace"},{ "commodityAsset" : "8a1cfae21368b8bebbbed9800fec304e95cce39a2a57dc35e2e3ebaa.4d494c4b", "currencyAsset" : "lovelace"},{ "commodityAsset" : "8cfd6893f5f6c1cc954cec1a0a1460841b74da6e7803820dde62bb78.524a56", "currencyAsset" : "lovelace"},{ "commodityAsset" : "8daefa391220bd0d8d007f3748d870f7f3c106040314c8515ccc35a5.464c4143", "currencyAsset" : "lovelace"},{ "commodityAsset" : "8db269c3ec630e06ae29f74bc39edd1f87c819f1056206e879a1cd61.446a65644d6963726f555344", "currencyAsset" : "lovelace"},{ "commodityAsset" : "8db269c3ec630e06ae29f74bc39edd1f87c819f1056206e879a1cd61.5368656e4d6963726f555344", "currencyAsset" : "lovelace"},{ "commodityAsset" : "8fef2d34078659493ce161a6c7fba4b56afefa8535296a5743f69587.41414441", "currencyAsset" : "lovelace"},{ "commodityAsset" : "95a427e384527065f2f8946f5e86320d0117839a5e98ea2c0b55fb00.48554e54", "currencyAsset" : "lovelace"},{ "commodityAsset" : "9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77.53554e444145", "currencyAsset" : "lovelace"},{ "commodityAsset" : "9abf0afd2f236a19f2842d502d0450cbcd9c79f123a9708f96fd9b96.454e4353", "currencyAsset" : "lovelace"},{ "commodityAsset" : "a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235.484f534b59", "currencyAsset" : "lovelace"},{ "commodityAsset" : "a3931691f5c4e65d01c429e473d0dd24c51afdb6daf88e632a6c1e51.6f7263666178746f6b656e", "currencyAsset" : "lovelace"},{ "commodityAsset" : "b34b3ea80060ace9427bda98690a73d33840e27aaa8d6edb7f0c757a.634e455441", "currencyAsset" : "lovelace"},{ "commodityAsset" : "b6a7467ea1deb012808ef4e87b5ff371e85f7142d7b356a40d9b42a0.436f726e75636f70696173205b76696120436861696e506f72742e696f5d", "currencyAsset" : "lovelace"},{ "commodityAsset" : "c0ee29a85b13209423b10447d3c2e6a50641a15c57770e27cb9d5073.57696e67526964657273", "currencyAsset" : "lovelace"},{ "commodityAsset" : "da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24.4c51", "currencyAsset" : "lovelace"},{ "commodityAsset" : "dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb.0014df1047454e53", "currencyAsset" : "lovelace"},{ "commodityAsset" : "e52964af4fffdb54504859875b1827b60ba679074996156461143dc1.4f5054494d", "currencyAsset" : "lovelace"},{ "commodityAsset" : "edfd7a1d77bcb8b884c474bdc92a16002d1fb720e454fa6e99344479.4e5458", "currencyAsset" : "lovelace"},{ "commodityAsset" : "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535.41474958", "currencyAsset" : "lovelace"},{ "commodityAsset" : "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880.69425443", "currencyAsset" : "lovelace"},{ "commodityAsset" : "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880.69455448", "currencyAsset" : "lovelace"},{ "commodityAsset" : "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880.69555344", "currencyAsset" : "lovelace"},{ "commodityAsset" : "fbae99b8679369079a7f6f0da14a2cf1c2d6bfd3afdf3a96a64ab67a.0014df1047454e5358", "currencyAsset" : "lovelace"},{ "commodityAsset" : "577f0b1342f8f8f4aed3388b80a8535812950c7a892495c0ecdf0f1e.0014df10464c4454", "currencyAsset" : "lovelace"},{ "commodityAsset" : "fc11a9ef431f81b837736be5f53e4da29b9469c983d07f321262ce61.4652454e", "currencyAsset" : "lovelace"},{ "commodityAsset" : "af2e27f580f7f08e93190a81f72462f153026d06450924726645891b.44524950", "currencyAsset" : "lovelace"},{ "commodityAsset" : "64f7b108bd43f4bde344b82587655eeb821256c0c8e79ad48db15d18.44454449", "currencyAsset" : "lovelace"},{ "commodityAsset" : "60faa64709fede8dffed0dccb69da337a5bf61eda0c773156ea78b4d.534b59", "currencyAsset" : "lovelace"},{ "commodityAsset" : "6a0f6c32b77596240ff8bc78e1c334fa9bd2131750f47f3afb36b1b2.4d4f4749", "currencyAsset" : "lovelace"},{ "commodityAsset" : "48f69a80dcca7bee58431b8b749faca98700ca456056d153fbea4eb2.4f4d4e49", "currencyAsset" : "lovelace"}]'

ENTRYPOINT ["/bin/bash", "./start.sh"]

# ==============================[ SECURITY UPDATES ]=================================
RUN apt-get update && apt-get upgrade -y && apt-get clean
