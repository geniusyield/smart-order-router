#!/bin/bash

FROM_BABBAGE=72316796.c58a24ba8203e7629422a24d9dc68ce2ed495420bf40d9dab124373655161a20
MATCH_ORDERS=22f6999d4effc0ade05f6e1a70b702c65d6b3cdf0e301e4a8267f585.*
MATCH_ORDERS_V1_1=642c1f7bf79ca48c0f97239fcb2f3b42b92f2548184ab394e1e1e503.*
MATCH_BOT=$1
MATCH_MINTING_POLICY_REF=1@062f97b0e64130bc18b4a227299a62d6d59a4ea852a4c90db3de2204a2cd19ea
MATCH_VALIDATOR_REF=2@062f97b0e64130bc18b4a227299a62d6d59a4ea852a4c90db3de2204a2cd19ea
MATCH_CONFIG_ADDR=addr1w9zr09hgj7z6vz3d7wnxw0u4x30arsp5k8avlcm84utptls8uqd0z
MATCH_MINTING_POLICY_REF_V1_1=1@c8adf3262d769f5692847501791c0245068ed5b6746e7699d23152e94858ada7
MATCH_VALIDATOR_REF_V1_1=2@c8adf3262d769f5692847501791c0245068ed5b6746e7699d23152e94858ada7
MATCH_CONFIG_ADDR_V1_1=addr1wxcqkdhe7qcfkqcnhlvepe7zmevdtsttv8vdfqlxrztaq2gge58rd

kupo \
	--node-socket $NODE_MAINNET/db/node.socket \
	--node-config $NODE_MAINNET/config.json \
	--since $FROM_BABBAGE \
	--match $MATCH_ORDERS \
	--match $MATCH_ORDERS_V1_1 \
	--match $MATCH_BOT \
	--match $MATCH_MINTING_POLICY_REF \
	--match $MATCH_VALIDATOR_REF \
	--match $MATCH_CONFIG_ADDR \
	--match $MATCH_MINTING_POLICY_REF_V1_1 \
	--match $MATCH_VALIDATOR_REF_V1_1 \
	--match $MATCH_CONFIG_ADDR_V1_1 \
	--prune-utxo \
	--workdir $NODE_MAINNET/kupo/sor/db
