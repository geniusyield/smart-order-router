<h1 align="center">Smart Order Router</h1>
<p align="center">
    <a href="https://github.com/geniusyield/smart-order-router/actions?query=branch%3Amain"><img src="https://img.shields.io/github/actions/workflow/status/geniusyield/smart-order-router/build.yml?style=flat-square&branch=main&label=Build" /></a>
    <a href="https://www.haskell.org/"><img alt="GitHub top language" src="https://img.shields.io/github/languages/top/geniusyield/smart-order-router?style=flat-square"></a>
    <a href="https://github.com/geniusyield/smart-order-router/commits/main"><img src="https://img.shields.io/github/commit-activity/m/geniusyield/smart-order-router?style=flat-square&label=Commit%20Activity" /></a>
    <a href="https://github.com/geniusyield/smart-order-router/blob/main/LICENSE"><img src="https://img.shields.io/github/license/geniusyield/smart-order-router?style=flat-square&label=Licence" /></a>
    <a href="./CONTRIBUTING.md"><img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square" /></a>
    <a href="https://twitter.com/GeniusyieldO"><img src="https://img.shields.io/badge/-%40GeniusYieldO-F3F1EF?style=flat-square&logo=twitter&logoColor=1D9BF0" /></a>
    <a href="https://discord.gg/TNHf4fs626"><img src="https://img.shields.io/badge/-Discord-414EEC?style=flat-square&logo=discord&logoColor=white" /></a>
  </p>

## Table of contents

- 🎓 [(Legacy) Partial Orders](#partial-orders)
- 🎓 [Two Way Orders](#two-way-orders)
- 🔍 [How bot assesses profitability](#how-bot-assesses-profitability)
- 🚀 [Building and running](#building-and-running-the-smart-order-router)
- 🧠 [Strategies](#strategies)
- 💰 [Yield Accelerator Rewards](#yield-accelerator-rewards)
- 🛠️ [Troubleshooting](#troubleshooting)
- ⚖️ [License](#license)

## Overview

Smart Order Routers play a crucial role in the operation of the [Genius Yield Decentralized Exchange](https://www.geniusyield.co/).

Smart Order Routers (SORs) are off-chain agents that execute a routing algorithm that scans 
the Cardano blockchain for open limit orders, matches them based on their trigger conditions,
and submits new transactions back to the ledger to perform the swap state transitions.
Each Smart Swap encodes trigger conditions that the SOR must fulfill to execute the swap.
The SOR continuously scans and analyzes the current state of the limit orders on-chain and relies on
the configured matching strategy to best execute a customer’s order based on price.

Specifically, the SOR periodically builds a multi-asset order book consisting of one
order book for each token pair listed in its configuration. Each order book contains
only sell and buy orders for the same pair of tokens. The bot runs the selected strategy
over the multi-asset order book to obtain a list of matches. The matches are then
translated into actual transactions that will be signed and submitted by the bot, if
these are profitable to execute.

Due to the open and decentralized design of the protocol, anybody can run a Smart Order
Router instance and profit from the arbitrage opportunities, thus running a Smart Order
Router instance is not only contributing to the further decentralization of the protocol,
but it is also incentivized financially and it might be a very lucrative activity.

## GeniusYield DEX Partial Orders

Let's start with a brief overview of the GY DEX Orders, to provide some context, so the reader could better understand
the purpose of the Smart Order Routers and how they could benefit from customizing existing order matching strategies
or even implementing completely new strategies from scratch.

For a more detailed description, please see the [Genius Yield Whitepaper](https://www.geniusyield.co/whitepaper.pdf?lng=en).

Given a pair of tokens, an order will contain the number of tokens it offers and the price of one unit of offered token in terms of asked tokens.
Besides that, the order will have some life timeline and, of course, a notion of ownership
related to the one that created it.

For example, we could create an order by offering `10 GENS`, with a unit price of `2 ADA`, so we expect to receive `2 ADA` for each `GENS` token.

Not only the amount of the offered tokens and the price, but the owner of the order must be set as well.

Let's say that the owner of the order in this case is the reader.

It's important to mention that all of these information is **mandatory**. The offered amount, the price and the owner must be all set.

Optionally it is possible to set a lifespan for the Order. Setting start and end time of the validity of the order can be used to created
an order that could be filled only during the timespan defined by these constraints.

Once we create an GY order on-chain by submitting a transaction, the offered tokens will be locked in the order. In the example above, the
`10 GENS` will be locked in the on-chain order on creation. 

Two different "actions" might be performed on an existing on-chain order: canceling and filling.

Only the owner might _cancel_ an order created by him and redeem the tokens that had been previously locked in the order instance on creation.

Anyone can _fill_ a particular on-chain order.

Filling an order means to pay the correct amount of tokens the owner of the order configured to receive for the offered tokens in exchange.

The Genius Yield DEX supports partial fills as well. This means that an Order might be filled in several steps and not only by a single
transaction.

For example in the case of the previous example, anyone could fill that order by buying `6 GENS` from it by paying `12 ADA`. Please note that
the remaining `4 GENS` are still going to be available for sale (for `2 ADA` each).

For the end user it is transparent that there are two kinds of order fills: _complete_ and _partial_.

A complete fill will buy all the offered tokens from the order, and for the partial fill, we need to specify the amount we want
to buy from the order.

For those who want to improve matching strategy implementation or implement completely new strategies,
this is highly relevant, since it is possible to design different matching strategies using these two different type of fills.

Up to this point, we quickly covered the key actions that can be performed over the orders.
There shouldn't be any surprise if we mention that each action is performed by a transaction.

Let's say that we have one more order offering of `20 GENS`, with a unit price of `2.5 ADA`.

We could earn some tokens by “combining” the two orders and take advantage of the price difference.

Following the example, given we bought `8 GENS` using `16 ADA`, we now can use these `8 GENS`
to buy back `20 ADA` from this other order, earning `4 ADA`.

These two fills can be combined into a single transaction, in fact, we could combine more than two orders.

The SOR can build these transactions matching orders programmatically, that is, combining orders into a single transaction.
Which orders the SOR will match is determined by the strategy that must be configured in advance.
To reason about any strategy, we need to classify orders into sell or buy. It's possible for an order to be a buy or sell,
depending on the token used to earn the difference between the orders.

In the previous example, we earned in `ADA`, but we could have earned in `GENS`.

So, given a token pair, we will specify which token is the commodity and which is the currency,
which will establish if a given order is a sell order or a buy order: If the order offers commodity,
then it will be classified as a sell order. On the other hand, if the order involves buying the
commodity with currency (that is offers currency), it will be considered a buy order.

Using the previous example we could have two cases:

<table align="center">
<tr><th> Commodity A | Currency B </th><th> Commodity B | Currency A </th></tr>
<tr><td>

|  Amount   |   Price   | Type  |
| :-------: | :-------: | :---: |
| `10 GENS` |  `2 ADA`  | Sell  |
| `8 GENS`  | `2.5 ADA` |  Buy  |

</td><td>

|  Amount  |   Price    | Type  |
| :------: | :--------: | :---: |
| `20 ADA` | `0.4 GENS` | Sell  |
| `20 ADA` | `0.5 GENS` |  Buy  |

</td></tr>
</table>

If we want our earnings to be in `GENS` then the commodity must be `ADA`. So we can buy from the sell order,
`20 ADA` using `8 GENS`, then using these `20 ADA` we can get `10 GENS` from the buy order, earning `2 GENS`.

## GeniusYield DEX Two Way Orders
The current flagship orders in GeniusYield DEX are are termed SLV (Smart Liquidity Vaults) or Two-Way Orders. They could be thought of as above described Partial Orders with the 2 additional functionalities:
- ability to set price based on current market exchange price of the token
- ability to set straight and reverse order simultaneously

Two-Way Orders are liquidity positions which are placed just like above mentioned DEX Orders but the market maker can allow trades on both tokens. For example, any order which specifies two-way trade, say between ADA and GENS can specify independent prices for both "buy ADA from GENS" and "buy GENS from ADA" trades. Additionally, while specifying prices the maker has the option to set a fixed price (say, 1 GENS for 2 ADA) but also a dynamic price (say, 1 GENS for current market exchange rate of GENS/ADA plus some offset and spread). The GeniusYield DEX exposes a web API which can be called to get the current price of any currency pair (one of which must be ADA). The certified price from GeniusYield DEX contains a certificate which must be included whenever one wants to fill an order using the Haskell API in `dex-contracts-api` repo.

In conclusion, with the introduction of SLV, the market maker is not required to run a bot to place and adjust separate bid and ask orders because only one two-way SLV with dynamic prices (set based on offset + spread) can "mimic" the market bot with dynamically changing positions.


## How bot assesses profitability?

Our design aims to execute built transactions only if they are "profitable" but how do we define profitability? We use the following rules regarding it:
* If currency of all the pairs (`scanTokens`) is set to ADA, then transaction is profitable if bot doesn't lose balance for any of it's tokens.
* For other case, since arbitrage isn't guaranteed to be in ADA but as transaction fees must be paid in ADA, some ADA loss might be inevitable. Here we require that token balance for any non-ada token does not decrease and that _ADA equivalent_ for arbitraged non-ADA token (along with any arbitraged ADA) compensates loss of ADA due to fees.
  * In case `tokenInfos`[^1] misses an entry for an arbitraged token or in case an error is encountered when obtaining price from provider, we make a log with high severity (warning in case of missed entry & error in case of remote price provider failure) and assume it's ADA equivalent value to be zero (to be on safe side) when determining above profitability check.
  * Note that in case `priceProvider` field is not provided in bot's configuration, profitability check is slightly modified where we require that bot doesn't lose any ADA besides transaction fees (along with requiring that it doesn't lose any non-ADA token).

## Building and running the Smart Order Router

### Running the SOR: System requirements

Minimum System Requirements:
- Memory: 500 MB
- CPU: 0.5vCPU (2.25 GHz CPU Base Frequency)
- Reliable and fast internet connection

Recommended System Requirements:
- Memory: 1GB
- CPU: 1vCPU (2.25 GHz CPU Base Frequency)
- Reliable and blasingly fast internet connection

The Smart Order Router does not require a lot of resources, but if you choose to use the Kupo provider and run the Cardano Node yourself, that needs much more resources, especially on the Cardano Mainnet.

For the exact requirements please see current Cardano Node documentation.

### Running the SOR using Docker

A ready-to-run, containerized version of the Smart Order Router is available via the [GitHub Container Registry](https://github.com/geniusyield/smart-order-router/pkgs/container/smart-order-router).

There are several options to run an SOR instance. One could use the service provided by Maestro and configure the SOR instance to use this
as backend provider. For such deployments a Maestro API key is needed.

A Maestro API key is available after registration via the following URL:
 - https://docs.gomaestro.org/Getting-started/Sign-up-login

As alternive; Blockfrost could be also used as backend provider. For Blockfrost backed SOR instances, a Blockfrost API key is needed.

The third option is to run your own Cardano Node and use Kupo to index the on-chain data.

For examples for all of these three please see the rest of the chapter.

A Smart Order Router container instance using the Maestro backend can be started by using the following snippet:

``` bash
# SMART ORDER INSTANCE ROUTER USING MAESTRO
# =========================================
# Replace these values with your configuration:
PAYMENT_SIGNING_KEY_CBOR_HEX=5820d682e237a04d43ad011fdecd141acd485f6d3d634466692d58f6d75250f39134
COLLATERAL_UTXO_REF=7cc7b044d26981d3fc73ae72994f289d99ba113ceefb5b83f4d7643bfb12682a#1
MAESTRO_API_KEY=some_api_key
CARDANO_NETWORK=preprod

docker run -it \
    -e BOTC_SKEY="{\"cborHex\": \"$PAYMENT_SIGNING_KEY_CBOR_HEX\", \"type\": \"PaymentSigningKeyShelley_ed25519\", \"description\": \"Payment Signing Key\"}" \
    -e BOTC_COLLATERAL="$COLLATERAL_UTXO_REF" \
    -e BOTC_CONFIG="{ \"coreProvider\": { \"maestroToken\": \"$MAESTRO_API_KEY\" }, \"networkId\": \"$CARDANO_NETWORK\", \"logging\": [{ \"type\": { \"tag\": \"stderr\" }, \"severity\": \"Info\", \"verbosity\": \"V2\" }],\"utxoCacheEnable\": false }" \
    ghcr.io/geniusyield/smart-order-router:latest
```

Please make sure to replace the placeholders with the actual values.

Alternatively the Blockfrost or the Kupo backend could be used.

This can be accomplished for Blockfrost by using the following commands:

> [!NOTE]
> Few of the optimisations that we make use of such as querying UTxOs and their datums in a single request, aren't available for Blockfrost, thus, this provider is expected to run slow compared to other providers.

``` bash
# SMART ORDER ROUTER INSTANCE USING BLOCKFROST
# ============================================
# Replace these values with your configuration:
PAYMENT_SIGNING_KEY_CBOR_HEX=5820d682e237a04d43ad011fdecd141acd485f6d3d634466692d58f6d75250f39134
COLLATERAL_UTXO_REF=7cc7b044d26981d3fc73ae72994f289d99ba113ceefb5b83f4d7643bfb12682a#1
BLOCKFROST_API_KEY=some_api_key
CARDANO_NETWORK=preprod

docker run -it \
    -e BOTC_SKEY="{\"cborHex\": \"$PAYMENT_SIGNING_KEY_CBOR_HEX\", \"type\": \"PaymentSigningKeyShelley_ed25519\", \"description\": \"Payment Signing Key\"}" \
    -e BOTC_COLLATERAL="$COLLATERAL_UTXO_REF" \
    -e BOTC_CONFIG="{ \"coreProvider\": { \"blockfrostKey\": \"$BLOCKFROST_API_KEY\" }, \"networkId\": \"$CARDANO_NETWORK\", \"logging\": [{ \"type\": { \"tag\": \"stderr\" }, \"severity\": \"Info\", \"verbosity\": \"V2\" }],\"utxoCacheEnable\": false }" \
    ghcr.io/geniusyield/smart-order-router:latest
```

And the following commands can be used to start a Kupo backed instance, if you want to use an existing Kupo instance:

``` bash
# SMART ORDER ROUTER INSTANCE USING KUPO (existing Kupo instance)
# ===============================================================
# Replace these values with your configuration:
PAYMENT_SIGNING_KEY_CBOR_HEX=5820d682e237a04d43ad011fdecd141acd485f6d3d634466692d58f6d75250f39134
COLLATERAL_UTXO_REF=7cc7b044d26981d3fc73ae72994f289d99ba113ceefb5b83f4d7643bfb12682a#1
KUPO_URL=http://some.url.to.your.kupo.instance:1442
CARDANO_NODE_SOCKET_PATH=/cardano/node/socket
CARDANO_NETWORK=preprod

docker run -it \
    -e BOTC_SKEY="{\"cborHex\": \"$PAYMENT_SIGNING_KEY_CBOR_HEX\", \"type\": \"PaymentSigningKeyShelley_ed25519\", \"description\": \"Payment Signing Key\"}" \
    -e BOTC_COLLATERAL="$COLLATERAL_UTXO_REF" \
    -e BOTC_CONFIG="{\"coreProvider\": { \"socketPath\": \"/cardano/node/socket\", \"kupoUrl\": \"$KUPO_URL\" }, \"networkId\": \"$CARDANO_NETWORK\", \"logging\": [{ \"type\": { \"tag\": \"stderr\" }, \"severity\": \"Info\", \"verbosity\": \"V2\" }], \"utxoCacheEnable\": false }" \
    -v $CARDANO_NODE_SOCKET_PATH:/cardano/node/socket \
    ghcr.io/geniusyield/smart-order-router:latest
```

or alternatively you could use docker-compose to start a Cardano node, a Kupo instance and a Smart Order Router instance using the created Kupo instance:

``` bash
# SMART ORDER ROUTER INSTANCE USING KUPO (docker-compose)
# =======================================================
# Replace these values with your configuration:
PAYMENT_SIGNING_KEY_CBOR_HEX=5820d682e237a04d43ad011fdecd141acd485f6d3d634466692d58f6d75250f39134 \
COLLATERAL_UTXO_REF=7cc7b044d26981d3fc73ae72994f289d99ba113ceefb5b83f4d7643bfb12682a#1 \
docker compose up
```

Kupo can be run in a docker container, but there is also a helper script to start an instance locally ([`kupo-preprod.sh`](./scripts/kupo-preprod.sh)).

  > [!NOTE]
  > **How to run Kupo efficiently?**
  >
  > Firstly, Kupo requires a node running, note that node itself maintains efficient access to information such as current protocol parameters, current set of pool ids, etc. but it doesn't efficiently provide us with UTxOs when say queried by a particular address. Kupo helps in covering this gap and gives us efficient lookup tables to query for UTxOs. For our use case, we are only interested in our own bot's UTxOs, order UTxOs and the required reference scripts / reference inputs. So we'll run Kupo to keep track of only those UTxOs, note that if we instead run Kupo by matching against star (`*`) pattern, then as Kupo does many disk writes, we would quickly burn out our SSDs TBW limit.
  >
  > Please see the scripts, [`kupo-preprod.sh`](./scripts/kupo-preprod.sh) for pre-production network and [`kupo-mainnet.sh`](./scripts/kupo-mainnet.sh) for mainnet network to see how this can be achieved. Note that these two scripts take as an argument the match pattern for bot's UTxOs, you may very well give the bech32 address of bot as value of this argument. To understand what all the script does, please see Kupo's [documentation](https://cardanosolutions.github.io/kupo/#section/Getting-started).

### Building the SOR using docker

The SOR can be built using docker.

Simply cloning the repository and building the docker image should be sufficient, so no Haskell tooling, like GHC or Cabal must be installed locally.

All of these tools necessary for building the SOR are available in the build stage of the [Dockerfile](https://github.com/geniusyield/smart-order-router/blob/main/Dockerfile).

```bash
git clone https://github.com/geniusyield/smart-order-router.git
cd smart-order-router
docker build .
```

Smaller changes to the logic might be possible using the the docker based build, but if bigger changes are necessary, you might want to build the SOR locally on you workstation directly.

In the next chapter you can find detailed step-by-step description about this.

### Locally building the SOR

First, you need to setup the necessary tooling to work with [haskell.nix](https://github.com/input-output-hk/haskell.nix).
A complete guide and troubleshooting of how to install and configure `nix` can be
found on one of the officials IOG repositories: [plutus-apps](https://github.com/input-output-hk/plutus-apps/blob/main/CONTRIBUTING.adoc#installing-and-setting-up-nix).
Once we completed the previous steps we can simply run `nix develop`, and it will
drop you into a shell with all the necessary tools. Once inside the environment,
you can build the order bot with `cabal build all`.

### Orderbot Config

To run the order bot, it is necessary to setup the provider and specify the bot options. The provider configuration defines how the SOR instances accesses the Cardano blockchain and how it is submitting transactions.

It is possible to use a completely autonomous local provider by utilizing [Kupo](https://github.com/CardanoSolutions/kupo/tree/master) and running your own [Cardano node](https://github.com/input-output-hk/cardano-node), but it is also possible to use one of the service providers to access the Cardano Blockchain; Maestro or Blockfrost, so you could rely on their services, which enables you to run your SOR instance with minimal resources needed and without running your own Cardano node.

#### Local Provider: using Kupo and a Cardano Node

[Kupo](https://github.com/CardanoSolutions/kupo) can be used as a local provider. For this it is necessary to provide a path to a cardano node socket file and the Kupo url in the [atlas-config-kupo.json](./config-files/atlas-config-kupo.json) file.

#### Remote Providers

There are two possible remote providers: `Maestro` or `Blockfrost`. In order to function properly, each provider requires a specific `API-TOKEN` or `API-KEY` (that should be created on each official site). These must be entered into the appropriate provider configuration file, either [atlas-config-maestro.json](./config-files/atlas-config-maestro.json) or [atlas-config-blockfrost.json](./config-files/atlas-config-blockfrost.json).

You must also configure the `networkId` to specify which Blockchain to use.
Inside any of those configuration files, we can also configure the logging mechanisms, we can
specify the log severity level with `Debug`, `Info`, `Warning`, but also the sinking of the information
by choosing between a console log or a file. Besides different levels of verbosity. We even can
have multiple logs, for instance on the example below we are logging some `Info` level info into de
console, and some `Debug` level info into the `Debug.log` file.

```json
"logging": [ { "type": { "tag": "stderr" }, "severity": "Info", "verbosity": "V2" }
           , { "type": { "tag": "gySource", "source" : "Debug.logs" }, "severity": "Debug", "verbosity": "V2" }
           ]
```

#### Bot configuration

In addition, to configure the **bot**, it is necessary to edit the [bot-config.json](./config-files/bot-config.json)
file. The complete bot configuration looks like this:

```json
{
   "signingKeyFP": "bot.skey",
   "strategy": "OneSellToManyBuy",
   "scanDelay": 40000000,
   "maxOrderMatches": 5,
   "maxTxsPerIteration": 4,
   "randomizeMatchesFound": true,
   "scanTokens": [
      {
         "commodityAsset": "c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53",
         "currencyAsset": "lovelace"
      }
   ],
   "lovelaceWarningThreshold": 5000000,
   "priceProvider": {
      "tag": "tapTools",
      "contents": {
         "apiKey": "YOUR_API_KEY"
      }
   },
   "tokenInfos": {
      "dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb.0014df1047454e53": {
         "ticker": "GENS",
         "decimals": 6
      },
      "c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad.0014df105553444d": {
         "ticker": "USDM",
         "decimals": 6
      }
   }
}
```
- `signingKeyFP`, we need to specify the bot signing key, that must be placed
  on a file.
- `collateral`, an optional field to specify the collateral for the bot. If not
  present, Atlas will choose a suitable UTxO as collateral.
- `strategy`, currently the SOR supports one possible strategy: OneSellToManyBuy.
- `scanDelay`, the duration of time in µs we wait before re-initiating a complete iteration for the bot.
- `maxOrderMatches`, is the maximum amount of orders to be matched into a single transaction.
  8 orders is near the limit that will fit into a transaction.
- `maxTxsPerIteration`, is the maximum amount of transactions that the bot will build,
  sign and submit in each iteration.
- `randomizeMatchesFound`, a boolean that dictates whether the bot chooses the tx
  to submit at random (to decrease collisions), or not (to maximize profit)
- `scanTokens`, the list of token pairs to be scanned. Each element in the list specifies
  which token in the pair is the `commodityAsset` and which is the `currencyAsset`. The bot
  will arbitrage the orders to get tokens of the `currencyAsset`. Each token must be written
  with the format policyId.hexTokenName. For convenience, scanning ADAs can be done by
  writing lovelace or the empty string. The multi-asset order book is built using this list.
- `lovelaceWarningThreshold`, denotes lovelace value. If bot's balance is below this threshold, log with warning severity is made.
- `priceProvider` (optional) is used to configure price provider which is used to obtain ADA price for a token. Currently two price providers are supported.
  - To use [Maestro's OHLCV](https://docs.gomaestro.org/cardano/dex-and-pair-ohlc) endpoint, example configuration is provided below. `resolution` & `dex` field correspond directly to underlying Maestro endpoint:
    ```json
    "priceProvider": {
      "tag": "maestro",
      "contents": {
        "apiKey": "YOUR_API_KEY",
        "resolution": "15m",
        "dex": "minswap"
      }
    }
    ```
  - To use [TapTools's prices](https://openapi.taptools.io/#tag/Market-Tokens/paths/~1token~1prices/post) endpoint, example configuration is provided below. Note that when using TapTools price provider, SOR does single request to obtain price information for multiple tokens when required as underlying endpoint supports price fetch for multiple assets in a single API call. Whereas for Maestro, request is made per individual token.
    ```json
    "priceProvider": {
      "tag": "tapTools",
      "contents": {
        "apiKey": "YOUR_API_KEY"
      }
    }
    ```
- `tokenInfos` (optional) is used to provide token's registered off-chain metadata. Since prices provided by providers utilise "display" units which is independent of underlying blockchain ledger, we require information such as token's registered decimal places to obtain lovelace value per token's indivisible unit.

> [!NOTE]
> See [_How bot assesses profitability_](#how-bot-assesses-profitability) section on how fields of `tokenInfos` & `priceProvider` are used to assess profitability.

#### Creating Signing Key

Another important and necessary setup to make is the creation of the bot wallet.
If you already have a wallet signing key to use, you just need to put the signing
key in a file and properly configure `signingKeyFP`.

If you want to create a new wallet, you can create everything using the `cardano-cli`:

```shell
cardano-cli address key-gen \
	--verification-key-file bot.vkey \
	--signing-key-file bot.skey

cardano-cli address build \
    --payment-verification-key-file bot.vkey \
    --testnet-magic 1 \
    --out-file bot.preprod.addr
```

This will create the files `bot.skey`, `bot.vkey`, and `bot.preprod.addr`:
the private signing key, the verification key, and the wallet address on the
preprod testnet. You can claim some **preprod** lovelaces using the
[faucet](https://docs.cardano.org/cardano-testnet/tools/faucet/).

Read the cbor hex for the private signing key using the following command:

```shell
cat bot.skey | jq -r '.cborHex'
```

This is the value assigned to `PAYMENT_SIGNING_KEY_CBOR_HEX` when running the SOR via Docker.

It's **recommended** to create and setup a `collateral`. A UTxO with 5 ADAs will
do the work. But as we mentioned the `collateral` config field is optional.

Use the following command to find and select the UTXO to be used for collateral:

```shell
cardano-cli query utxo --address $(cat bot.preprod.addr) --mainnet
```

(For running in preprod, replace `--mainnet` with `--testnet-magic 1`)

Assign the selected UTXO (hash + index) to `COLLATERAL_UTXO_REF`  when running the SOR via Docker.

Alternatively, the UTXO information can also be obtained from a block explorer service such as https://cexplorer.io/

#### Running

Once we compiled and configured the order bot, you can execute the SOR using the [Makefile](./Makefile):
`make orderbot-maestro`, `make orderbot-blockfrost` or `make orderbot-kupo` depending on the provider you want to use.

#### Testing

The SOR is equipped with a test suite that employs QuickCheck to perform property-based testing.
By implementing certain properties, we are able to verify various important aspects of the strategies,
like for example, given a matching between sell and buy orders there is always a non-negative earning.
Among others that can be found on [Tests.Prop.Strategies](./geniusyield-orderbot/test/Tests/Prop/Strategies.hs)
module.

For running the tests we can just simply execute `make orderbot-tests`.

## Design

The SOR is organized into 5 main folders:

- [`geniusyield-orderbot-framework`](./geniusyield-orderbot-framework), implement the main abstract tools for the SOR.
- [`impl`](./impl), specific implementations of the orderbook, data-provider and strategies.
- [`geniusyield-orderbot`](./geniusyield-orderbot), simply runs the executable.

### Backpack

This is an order matching bot implementation that is meant to be modular and polymorphic. It
uses backpack to reach this goal. Backpack is extremely flexible, supporting signature
merging and signature thinning. This may be especially relevant for modular orderbot implementations.
[Signature thinning](https://github.com/danidiaz/really-small-backpack-example/tree/master/lesson4-signature-thinning) is when an indefinite library depends on a signature but only demands a
subset of said signature, allowing an implementation that only implements said subset of the
interface to be used merrily with the library.

To get started with Backpack, please see the following example: [A really small example of the Backpack module system for Haskell](https://github.com/danidiaz/really-small-backpack-example)

## Strategies

On the [`GeniusYield.OrderBot.Strategies.Impl`](./impl/strategies-impl/GeniusYield/OrderBot/Strategies/Impl.hs) module, you can find all the strategies
implemented by the SOR. Currently, there is only one called `OneSellToManyBuy`,
which basically takes the best sell order (the one with the lowest price) and searches for many buy
orders (starting from the one with the highest price), ideally buying the total amount of offered
tokens, or until it reaches the maxOrderMatches.

### Adding a new strategy

In this Haskell implementation, a strategy is simply a function with type `OrderAssetPair -> OrderBook -> [MatchResult]`.
This function returns a list of matching results from a pair of tokens and an order book,
which consists of both sell and buy orders. Each matching result represents a transaction,
which involves a specific set of sell and buy orders.

We can start with the most bureaucratic part of adding a new strategy. We need to define the
name of the new strategy, so let's say we want to implement the "reverse" strategy to the one
that is already there. We want to implement a strategy that takes the best buy order,
searches for many sell orders to match this with. We need to simply add a new constructor `OneBuyToManySell` to the 
`BotStrategy` type

```haskell
data BotStrategy = OneSellToManyBuy
                 | OneBuyToManySell
```

We must adjust some straightforward instances with the new constructor: `FromJSON` and `Var`.
As is the case with `mkIndependentStrategy`,
adding a new particular case for `OneBuyToManySell`

```haskell
mkIndependentStrategy :: BotStrategy -> Natural -> IndependentStrategy
mkIndependentStrategy bs maxOrders _ bk =
    case bs of
      OneSellToManyBuy -> oneSellToManyBuy maxOrders bk
      OneBuyToManySell -> oneBuyToManySell maxOrders bk
```

Once we get to this point, we can focus on the implementation of the new function. In fact,
we can just start with a dummy implementation that won't find any matching with the goal
to just to simply compile everything for now.

```haskell
oneBuyToManySell :: Natural -> OrderBook -> [MatchResult]
oneBuyToManySell _ _ = []
```

Even more! We can add the new constructor `OneBuyToManySell` to the `allStrategies` list
and this should be enough to start testing with our custom strategy by running the tests.

```haskell
allStrategies :: [BotStrategy]
allStrategies = [OneSellToManyBuy, OneBuyToManySell]
```

Finishing the dummy implementation of `oneBuyToManySell` with the actual logic is left as an optional coding exercise for the reader.

<details>
  <summary>Hint</summary>

> Checking `multiFill`,
  can help to realize that it's enough to use [`oneSellToManyBuy`]
  as inspiration and "flip" something.
</details>

Questions: Choosing between one strategy or the other will always enforce some matching strategy, so
will it be possible to merge the two strategies into a single one? Or it will be better to run two
different SOR instances?

## Yield Accelerator Rewards

SOR's fill orders and therefore participate in the GeniusYield Yield Accelerator Program and accumulate rewards.

Traders wishing to check and claim their rewards can easily do so in the [GeniusYield UI](https://app.geniusyield.co/earn),
but unfortunately, at the moment, the UI only works for users who connect their wallets to the UI and are identified by
the wallet stake key hash.

An SOR, on the other hand, normally just uses a simple payment signing key and an associated address without staking component.

To allow SOR operators to check and claim rewards, we are providing two simple bash scripts,
one for [checking](./scripts/check-bot-rewards.sh) and one for [claiming](./scripts/claim-bot-rewards.sh) rewards.
Both scripts require the `cardano-cli` to be installed and available in the `PATH`, and in order to claim,
you additionally need a connection to a running Cardano node.

To check your rewards, run the following command:

```shell
./scripts/check-bot-rewards.sh <PATH_TO_YOUR_PAYMENT_SIGNING_KEY>
```

To claim your rewards, run the following command:

```shell
./scripts/claim-bot-rewards.sh <PATH_TO_YOUR_PAYMENT_SIGNING_KEY> <PATH_TO_YOUR_CARDANO_NODE_SOCKET>
```

## Monitoring
There are several way you could keep an eye on the performance of your SOR instance.
- You could look up the address of your SOR on any blockchainexplorer like [cexplorer](https://cexplorer.io/) or [cardanoscan](https://cardanoscan.io/) and keep an eye on the transactions manually.
- You could automate this by using the [Thoth Cardano Bot](https://github.com/DevStakePool/thoth-bot) for Telegram, that you can configure to send you notifications every time your SOR is able to succesfully submit a match and you are the lucky one to be the one who is getting the rewards.
- Or just use the community [developed script](https://github.com/cloudstake/public/blob/main/genius-yield-errors.sh). Please note: the script had been developed by the community and it is not maintained by the Genius Yield development team.

One could also use the regular expressions from the [script](https://github.com/cloudstake/public/blob/main/genius-yield-errors.sh) to implement some monitoring using the tool of your choosing.

Cloud service providers like AWS, Google Cloud Platform or Azure offer monitoring suites that could be used for this purpose.

## Troubleshooting

### Provider related error messages

- `geniusyield-orderbot-exe: MspvApiError "SystemStart" (MaestroApiKeyMissing "Invalid authentication credentials")`,
  you need to setup the corresponding Maestro token into [atlas-config-maestro.json](./config-files/atlas-config-maestro.json) file.

- `geniusyield-orderbot-exe: BlpvApiError "LedgerGenesis" (BlockfrostTokenMissing "Invalid project token.")`
  you need to setup the corresponding Blockfrost token into [atlas-config-blockfrost.json](./config-files/atlas-config-blockfrost.json) file.

### Cardano related error messages

- `BadInputsUTxO` in the exception that is raised during tx submission, not creation/balancing,
  usually indicates contention. An order you are trying to match is being matched by another transaction.

- `ExUnitsTooBigUTxO` in the exception means you are trying to match too many orders simultaneously,
  making the transaction size cross the limit.

- `BalancingErrorInsufficientFunds` in the exception indicates there are not enough of one or many
  tokens to construct the transaction. If you see ADA in the value that is printed afterward, it means
  your bot is out of ADA. More often, however, this error will be raised if your matching strategy
  does not return proper order matches and there aren't enough tokens in the transaction bucket to pay
  for an order.

- `GYTxMonadException "... amount x must be smaller than offered amount x ...`,
  you are trying to partially fill an order, but the partial fill amount is the max volume of the
  order. Use [`CompleteFill`](./geniusyield-orderbot-framework/src/GeniusYield/OrderBot/MatchingStrategy.hs#L98C17-L98C29) instead. See [GeniusYield.OrderBot.MatchingStrategy](./geniusyield-orderbot-framework/src/GeniusYield/OrderBot/MatchingStrategy.hs#L98)
  for more information.

### Cabal and Haskell

- HLS will not work in signature modules, nor will it work in modules importing a signature module.

- Cabal mixins can be flaky sometimes. If you're sure you're using mixins properly but still
  getting "Module X does not require Y", try cleaning the geniusyield-orderbot build and rebuilding
  from scratch.

## License

[Apache-2.0](./LICENSE) © [GYELD GMBH](https://www.geniusyield.co).

[^1]: See ["Bot configuration"](#bot-configuration) section for elaboration on these fields.
