<h1 align="center">Smart Order Router</h1>
<p align="center">
    <a href="https://github.com/geniusyield/smart-order-router/actions?query=branch%3Amain">
      <img src="https://img.shields.io/github/actions/workflow/status/geniusyield/smart-order-router/build.yml?style=flat-square&branch=main&label=Build" />
    </a>
    <a href="https://www.haskell.org/">
      <img alt="GitHub top language" src="https://img.shields.io/github/languages/top/geniusyield/smart-order-router">
    </a>
    <a href="https://github.com/geniusyield/smart-order-router/commits/main">
      <img src="https://img.shields.io/github/commit-activity/m/geniusyield/smart-order-router?style=flat-square&label=Commit%20Activity" />
    </a>
    <a href="https://github.com/geniusyield/smart-order-router/blob/main/LICENSE">
      <img src="https://img.shields.io/github/license/geniusyield/smart-order-router?style=flat-square&label=Licence" />
    </a>
    <a href="./CONTRIBUTING.md">
      <img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square" />
    </a>
    <a href="https://twitter.com/GeniusyieldO">
      <img src="https://img.shields.io/badge/-%40GeniusYieldO-F3F1EF?style=flat-square&logo=twitter&logoColor=1D9BF0" />
    </a>
    <a href="https://discord.gg/TNHf4fs626">
      <img src="https://img.shields.io/badge/-Discord-414EEC?style=flat-square&logo=discord&logoColor=white" />
    </a>
  </p>

## Table of contents

- 🎓 [Crash Course](#crash-course-geniusyield-dex-orders--sor)
- 🚀 [Building and running](#building-and-running)
- 🧠 [Strategies](#strategies)
- 🛠️ [Troubleshooting](#troubleshooting)
- ⚖️ [License](#license)

## Overview

SORs are off-chain bots that execute a routing algorithm that scans the blockchain
for open limit orders, matches them based on their trigger conditions, and submits
new transactions back to the ledger to perform the swap state transitions. Each Smart
Swap encodes trigger conditions that the SOR must fulfill to execute the swap. The SOR
continuously scans and analyzes the current state of the limit orders and relies on
the configured matching strategy to best execute a customer’s order based on price.

Specifically, the bot periodically builds a multi-asset order book consisting of one
order book for each token pair listed in its configuration. Each order book contains
only sell and buy orders for the same pair of tokens. The bot runs the selected strategy
over the multi-asset order book to obtain a list of matches. The matches are then
translated into transactions that will be signed and submitted by the bot.

## Crash Course GeniusYield DEX Orders <> SOR

Let's start with a concrete and short overview of the GY DEX Orders, so the context
of the SOR for using, modifying, and improving with new custom strategies is properly
established. A complete description can be found in the [GY whitepaper](https://www.geniusyield.co/whitepaper.pdf?lng=en).

Given a pair of tokens, an order will contain the number of tokens it offers, the price
of one unit of those offered, and the minimal amount we are forced to buy from the order.
Besides that, the order will have some life timeline and, of course, a notion of ownership
related to the one that created it. For example, we could create an order offering of `10 tokenA`,
with a unit price of `2 tokenB`, that is, we expect to receive `2 tokenB` per `1 tokenA`. Also, we
want the minimal amount to be bought be `5 tokenA`. Clearly, the owners of this order will be
us and it's important to mention that all this information is **mandatory**, but we can avoid setting
the life timeline, meaning the order will always be available. Once we create an order, the offered
tokens will be locked on the order.

Given an order, two interesting "actions" can be performed over it. The owner can cancel
it and get back the locked tokens. Or anyone can _fill_ it, filling an order is just paying
the correct amount of tokens the owner of the order expects to receive related to the
amount of tokens we want to buy from that order. Following the previous example, anyone
could fill that order by buying from it `6 tokenA` and paying the owner `12 tokenB`. But, it
isn't possible to buy, for instance, `3 tokenA` from the order because the minimal amount
was setup to `5`, except the amount of offered tokens is less than that.

One important thing to mention that is transparent for any end user, is that there are
two kinds of fills: _complete_ and _partial_. A complete fill will buy all the offered
tokens from the order, and for the partial fill, we need to specify the amount we want
to buy from the order. For us, that will be running and probably improving this implementation
is relevant because, as we will see in a moment, we can design different matching strategies
using these two different fills.

Up to this point, we quickly covered the key actions that can be performed over the orders.
There shouldn't be any surprise if we mention that each action is performed by a transaction.

Now, let's suppose, besides the previous order, we have another one offering of `20 tokenB`,
with a unit price of `0.4 tokenA`. We could earn some tokens by “combining” the two orders
and take advantage of the price difference. Following the example, given we bought `6 tokenA`
using `12 tokenB`, we now can use these `6 tokenA` to buy back `15 tokenB` from this other
order, earning `3 tokenB`. These two fills can be combined into a single transaction, in
fact, we could combine more than two orders.

The SOR has the ability to build these transactions matching orders programmatically,
that is, combining orders into a single transaction. Which orders the SOR will match is
determined by the strategy that must be configured in advance. To reason about any strategy,
we need to classify orders into sell or buy. It's possible for an order to be a buy or sell,
depending on the token used to earn the difference between the orders. In the previous example,
we earned in `tokenB`, but we could have earned in `tokenA`. So, given a token pair, we will
specify which token is the commodity and which is the currency, which will establish if a
given order is a sell order or a buy order: If the order offers commodity, then it will
be classified as a sell order. On the other hand, if the order involves buying the
commodity with currency (that is offers currency), it will be considered a buy order.

Using the previous example we could have two cases:

<table align="center">
<tr><th> Commodity A | Currency B </th><th> Commodity B | Currency A </th></tr>
<tr><td>

|  Amount   |    Price   |  Type |
|:-----------:|:-----------:|:------:|
| `10 tokenA` |  `2 tokenB`   | Sell  |
| `8 tokenA`  |  `2.5 tokenB` | Buy   |

</td><td>

|  Amount   |    Price   |  Type |
|:-----------:|:-----------:|:------:|
| `20 tokenB` |  `0.4 tokenA`  | Sell  |
| `20 tokenB`  |  `0.5 tokenA` | Buy   |

</td></tr>
</table>

If we want our earnings to be in `tokenB`, then the commodity must be `tokenA`. We can buy from
the sell order, `6 tokenA` using `12 tokenB`, then using these `6 tokenA` we buy back `15 tokenB` from
the buy order, earning `3 tokenB`. However, if we want our earnings to be in `tokenA`, then the
commodity must be `tokenB`. So we can buy from the sell order, `18 tokenB` using `7 tokenA`, then
using these `18 tokenB` we buy back `9 tokenA` from the buy order, earning `2 tokenA`.

## Building and running

> [!NOTE]
> The Genius Yield DEX is in the public testnet phase at the moment.
>
> In order to run Smart Order Router instances for the public testnet, please use the preprod testnet as in the examples below.

### Docker

A ready-to-run, containerized version of the Smart Order Router is availabe via the [GitHub Container Registry](ghcr.io/geniusyield/smart-order-router:latest).

A Smart Order Router container instance using the Maestro backend can be started by using the following snippet:

``` bash
# SMART ORDER INSTANCE ROUTER USING MAESTRO
# =========================================
# Replace these values with your configuration:
PAYMENT_SIGNING_KEY_CBOR_HEX=5820d682e237a04d43ad011fdecd141acd485f6d3d634466692d58f6d75250f39134
COLLATERAL_UTXO_REF=7cc7b044d26981d3fc73ae72994f289d99ba113ceefb5b83f4d7643bfb12682a#1
MAESTRO_API_KEY=some_api_key
CARDANO_NETWORK=testnet-preprod

docker run -it \
    -e BOTC_SKEY="{\"cborHex\": \"$PAYMENT_SIGNING_KEY_CBOR_HEX\", \"type\": \"PaymentSigningKeyShelley_ed25519\", \"description\": \"Payment Signing Key\"}" \
    -e BOTC_COLLATERAL="$COLLATERAL_UTXO_REF" \
    -e BOTC_CONFIG="{ \"coreProvider\": { \"maestroToken\": \"$MAESTRO_API_KEY\" }, \"networkId\": \"$CARDANO_NETWORK\", \"logging\": [{ \"type\": { \"tag\": \"stderr\" }, \"severity\": \"Info\", \"verbosity\": \"V2\" }],\"utxoCacheEnable\": false }" \
    ghcr.io/geniusyield/smart-order-router:latest
```

Please make sure to replace the placeholders with the actual values.

Alternatively the Blockfrost or the Kupo backend could be used.

This can be accomplished for Blockfrost by using the following commands:

``` bash
# SMART ORDER ROUTER INSTANCE USING BLOCKFROST
# ============================================
# Replace these values with your configuration:
PAYMENT_SIGNING_KEY_CBOR_HEX=5820d682e237a04d43ad011fdecd141acd485f6d3d634466692d58f6d75250f39134
COLLATERAL_UTXO_REF=7cc7b044d26981d3fc73ae72994f289d99ba113ceefb5b83f4d7643bfb12682a#1
BLOCKFROST_API_KEY=some_api_key
CARDANO_NETWORK=testnet-preprod

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
CARDANO_NETWORK=testnet-preprod

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

### Local build

First, you need to setup the necessary tooling to work with [haskell.nix](https://github.com/input-output-hk/haskell.nix).
A complete guide and troubleshooting of how to install and configure `nix` can be
found on one of the officials IOG repositories: [plutus-apps](https://github.com/input-output-hk/plutus-apps/blob/main/CONTRIBUTING.adoc#installing-and-setting-up-nix).
Once we completed the previous steps we can simply run `nix develop`, and it will
drop you into a shell with all the necessary tools. Once inside the environment,
you can build the order bot with `cabal build all`.

### Orderbot Config

To run the order bot, it is necessary to setup the provider and specify the bot options. There is one option for a completely local provider and two remote ones.

#### Local Provider

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

In addition, to configure the **bot**, it is necessary to edit the [bot-config.json](./config-files/bot-config.json)
file. The complete bot configuration looks like this:

```json
{
   "signingKeyFP":"bot.skey",
   "nftMintingPolicyFP":"compiled-scripts/minting-policy",
   "orderValidatorFP":"compiled-scripts/partial-order",
   "validatorRefs":{
      "refAddr":"addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt",
      "refNftAC":"fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.e6a295bb83d06f53fcf91151f54acec0a63fbd6f0d924206d5d012e6da3b72af",
      "refNftUtxoRef":"39f987a6beb9cc4c45bba149a21c28068f640f3593f15f8157f0b6022b431977#0",
      "scriptRef":"39f987a6beb9cc4c45bba149a21c28068f640f3593f15f8157f0b6022b431977#1",
      "nftPolicyRef":"39f987a6beb9cc4c45bba149a21c28068f640f3593f15f8157f0b6022b431977#0"
   },
   "strategy":"OneSellToManyBuy",
   "scanDelay":40000000,
   "maxOrderMatches":5,
   "maxTxsPerIteration":5,
   "randomizeMatchesFound":true,
   "scanTokens":[
      {
         "commodityAsset":"c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53",
         "currencyAsset":"lovelace"
      }
   ]
}
```
- `signingKeyFP`, we need to specify the bot signing key, that must be placed
  on a file.
- `collateral`, an optional field to specify the collateral for the bot. If not
  present, Atlas will choose a suitable UTxO as collateral.
- `nftMintingPolicyFP` and `orderValidatorFP`, the filepath where the minting policy
  and validator are stored.
- `"validatorsRefs"`, contains the information necessary for the validator. Contains 3
  mandatory fields and 2 optional ones.
  - `"refAddr"`, address where the reference NFT is placed.
  - `"refNftAc"`, assetClass of the reference NFT is placed.
  - `"refNftUtxoRef"`, UTxO reference where the reference NFT is placed.
  - `"scriptRef"`, an optional parameter for the script reference. This UTxO has to
    have the partial order validator as a script ref.
  - `"nftPolicyRef"`, an optional parameter for the script reference of the partial
    orders NFT. This UTxO has to have the partial order NFT minting policy as a script ref.
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

It's **recomended** to create and setup a `collateral`. A UTxO with 5 ADAs will
do the work. But as we mentioned the `collateral` config field is optional.

#### Deployed Contract

The SOR has the ability to use reference scripts on the filling transactions to
help minimize the fees. To do that, we need to use the official contract information
that is completely placed on the blockchain. That is the validator and minting policy.

##### Preprod
```json
{
   "validatorRefs":{
      "refAddr":"addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt",
      "refNftAC":"fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.e6a295bb83d06f53fcf91151f54acec0a63fbd6f0d924206d5d012e6da3b72af",
      "refNftUtxoRef":"39f987a6beb9cc4c45bba149a21c28068f640f3593f15f8157f0b6022b431977#0",
      "scriptRef":"39f987a6beb9cc4c45bba149a21c28068f640f3593f15f8157f0b6022b431977#1",
      "nftPolicyRef":"39f987a6beb9cc4c45bba149a21c28068f640f3593f15f8157f0b6022b431977#0"
   }
}
```

##### Mainnet

> [!NOTE]
> The Smart Order Router configuration for the Cardano Mainnet is going to be available as we approach the Mainnet launch date.

```json
{
   "validatorRefs":{
      "refAddr":"",
      "refNftAC":"",
      "refNftUtxoRef":"",
      "scriptRef":"",
      "nftPolicyRef":""
   }
}
```

#### Running

Once we compiled and configured the order bot, you can execute using the [Makefile](./Makefile):
`make orderbot-maestro`, `make orderbot-blockfrost` or `make orderbot-kupo`.

#### Testing

The SOR is equipped with a test suite that employs QuickCheck to perform property-based testing.
By implementing certain properties, we are able to verify various important aspects of the strategies,
like for example, given a matching between sell and buy orders there is always a [positive earning](./geniusyield-orderbot/test/Tests/Prop/Strategies.hs#L167-L177).
Among others that can be found on [Tests.Prop.Strategies](./geniusyield-orderbot/test/Tests/Prop/Strategies.hs)
module.

For running the tests we can just simply execute `make orderbot-tests`.

## Design

The SOR is organized into 5 main folders:

- [`compiled-scripts`](./compiled-scripts), contains the compiled validator and minting policy of the DEX.
- [`geniusyield-dex-api`](./geniusyield-dex-api), defines the DEX interface to query and build transactions.
- [`geniusyield-orderbot-framework`](./geniusyield-orderbot-framework), implement the main abstract tools for the SOR.
- [`geniusyield-orderbot`](./geniusyield-orderbot), the executable is implemented here, together with the strategies.
- [`impl`](./impl), specific implementations of the orderbook and data-provider.

### Backpack

This is an order matching bot implementation that is meant to be modular and polymorphic. It
uses backpack to support this goal. Backpack is surprisingly flexible, supporting signature
merging and signature thinning. This may be especially relevant for modular orderbot implementations.
[Signature thinning](https://github.com/danidiaz/really-small-backpack-example/tree/master/lesson4-signature-thinning) is when an indefinite library depends on a signature but only demands a
subset of said signature, allowing an implementation that only implements said subset of the
interface to be used merrily with the library.

Solid resource for learning backpack: [GitHub - danidiaz/really-small-backpack-example: A really small example of the Backpack module system for Haskell](https://github.com/danidiaz/really-small-backpack-example)

## Strategies

On the [`Strategies`](./geniusyield-orderbot/src/Strategies.hs) module, you can find all the strategies
implemented by the SOR. Currently, there is only one called [`OneSellToManyBuy`](./geniusyield-orderbot/src/Strategies.hs#L36C20-L36C36),
which basically takes the best sell order (the one with the lowest price) and searches for many buy
orders (starting from the one with the highest price), ideally buying the total amount of offered
tokens, or until it reaches the maxOrderMatches.

### Adding a new strategy

In this Haskell implementation, a strategy is simply a function with type `OrderAssetPair -> OrderBook -> [MatchResult]`.
This function returns a list of matching results from a pair of tokens and an order book,
which consists of both sell and buy orders. Each matching result represents a transaction,
which involves a specific set of sell and buy orders.

We can start with the most bureaucratic part of adding a new strategy. We need to define the
name of the new strategy, so let's say we want to implement the "dual" strategy to the one
that is already there. We want to implement then one strategy that given the best buy order,
searches for many sell orders. We add then a new constructor `OneBuyToManySell` to the type
BotStrategy

```haskell
data BotStrategy = OneSellToManyBuy
                 | OneBuyToManySell
```

We must adjust some straightforward instances with the new constructor: `FromJSON` and `Var`.
As is the case with [`mkIndependentStrategy`](./geniusyield-orderbot/src/Strategies.hs#L56-L59),
adding a new particular case for `OneBuyToManySell`

```haskell
mkIndependentStrategy :: BotStrategy -> Natural -> IndependentStrategy
mkIndependentStrategy bs maxOrders _ bk =
    case bs of
      OneSellToManyBuy -> oneSellToManyBuy maxOrders bk
      OneBuyToManySell -> oneBuyToManySell maxOrders bk
```

Once we get to this point, we can focus on the implementation of the new function. In fact,
we can start with a very silly implementation that doesn't find any matching with the goal
of compiling everything.

```haskell
oneBuyToManySell :: Natural -> OrderBook -> [MatchResult]
oneBuyToManySell _ _ = []
```

Even more! We can go to the [Main](./geniusyield-orderbot/test/Main.hs) testing module,
and add the new constructor `OneBuyToManySell` to ["Strategies tests" list](./geniusyield-orderbot/test/Main.hs#L25C9-L25C29)
and it will be enough to start testing our strategy by running the tests.

Finishing the implementation of `oneBuyToManySell` is left as an exercise.

<details>
  <summary>Hint</summary>

> Checking [`multiFill`](./geniusyield-orderbot/src/Strategies.hs#L95-L132),
  can help to realize that it's enough to use [`oneSellToManyBuy`](./geniusyield-orderbot/src/Strategies.hs#L82-L92)
  as inspiration and "flip" something.
</details>

Questions: Choosing between one strategy or the other will always enforce some matching strategy, so
will it be possible to merge the two strategies into a single one? Or it will be better to run two
different SOR instances?

## Troubleshooting

### Providers

- `geniusyield-orderbot-exe: MspvApiError "SystemStart" (MaestroApiKeyMissing "Invalid authentication credentials")`,
  you need to setup the corresponding Maestro token into [atlas-config-maestro.json](./config-files/atlas-config-maestro.json) file.

- `geniusyield-orderbot-exe: BlpvApiError "LedgerGenesis" (BlockfrostTokenMissing "Invalid project token.")`
  you need to setup the corresponding Blockfrost token into [atlas-config-blockfrost.json](./config-files/atlas-config-blockfrost.json) file.

### Cardano

- `BadInputsUTxO` in the exception that is raised during tx submission, not creation/balancing,
  usually indicates contention. An order you are trying to match is being matched by another transaction.

- `ExUnitsTooBigUTxO` in the exception means you are trying to match too many orders simultaneously,
  making the transaction size cross the limit.

- `BalancingErrorInsufficientFunds` in the exception indicates there are not enough of one or many
  tokens to construct the transaction. If you see ADA in the value that is printed afterward, it means
  your bot is out of ADA. More often, however, this error will be raised if your matching strategy
  does not return proper order matches and there aren't enough tokens in the transaction bucket to pay
  an order.

- `GYTxMonadException "partiallyFillPartialOrder: amount x must be smaller than offered amount x`,
  you are trying to partially fill an order, but the partial fill amount is the max volume of the
  order. Use [`CompleteFill`](./geniusyield-orderbot-framework/src/GeniusYield/OrderBot/MatchingStrategy.hs#L98C17-L98C29) instead. See [GeniusYield.OrderBot.MatchingStrategy](./geniusyield-orderbot-framework/src/GeniusYield/OrderBot/MatchingStrategy.hs#L98)
  for more information.

### Cabal <> Haskell

- HLS will not work in signature modules, nor will it work in modules importing a signature module.

- Cabal mixins can be flaky sometimes. If you're sure you're using mixins properly but still
  getting "Module X does not require Y", try cleaning the geniusyield-orderbot build and rebuilding
  from scratch.

## License

[Apache-2.0](./LICENSE) © [GYELD GMBH](https://www.geniusyield.co).
