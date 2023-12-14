#!/bin/bash

FROM_BABBAGE=3542390.f93e682d5b91a94d8660e748aef229c19cb285bfb9830db48941d6a78183d81f
MATCH_ORDERS=158f42b49e0841301b45358b87744167f43359cc3785eab8d30893e1.*
MATCH_BOT=$1
MATCH_MINTING_POLICY_REF=1@be6f8dc16d4e8d5aad566ff6b5ffefdda574817a60d503e2a0ea95f773175050
MATCH_VALIDATOR_REF=2@be6f8dc16d4e8d5aad566ff6b5ffefdda574817a60d503e2a0ea95f773175050
MATCH_CONFIG_ADDR=addr_test1wrgvy8fermjrruaf7fnndtmpuw4xx4cnvfqjp5zqu8kscfcvh32qk

kupo \
	--node-socket $NODE_PREPROD/db/node.socket \
	--node-config $NODE_PREPROD/config.json \
	--since $FROM_BABBAGE \
	--match $MATCH_ORDERS \
	--match $MATCH_BOT \
	--match $MATCH_MINTING_POLICY_REF \
	--match $MATCH_VALIDATOR_REF \
	--match $MATCH_CONFIG_ADDR \
	--prune-utxo \
	--workdir $NODE_PREPROD/kupo/sor/db
