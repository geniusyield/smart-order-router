#!/bin/bash

echo "node socket path: $2"
echo "bot payment signing key file: $1"

VKEY_TMP_FILE=$(mktemp)
cardano-cli key verification-key --signing-key-file "$1" --verification-key-file "$VKEY_TMP_FILE"

PKH_TMP_FILE=$(mktemp)
cardano-cli address key-hash --payment-verification-key-file "$VKEY_TMP_FILE" --out-file "$PKH_TMP_FILE"
PKH=$(cat "$PKH_TMP_FILE")
echo "bot pubkey hash: $PKH"

ADDR_TMP_FILE=$(mktemp)
cardano-cli address build --mainnet --payment-verification-key-file "$VKEY_TMP_FILE" --out-file "$ADDR_TMP_FILE"
ADDR=$(cat "$ADDR_TMP_FILE")
echo "bot address (Bech32): $ADDR"

CBOR=$(cardano-cli address info --address "$ADDR" | jq .base16)
echo "bot address (CBOR): $CBOR"

rm "$VKEY_TMP_FILE"
rm "$PKH_TMP_FILE"
rm "$ADDR_TMP_FILE"

curl 'https://api.geniusyield.co/user/connect' \
  -H 'content-type: application/json' \
  --data-raw "{\"walletStakeKeyHash\":\"$PKH\",\"userType\":\"INDIVIDUAL\"}" \
  --compressed > /dev/null 2>&1

DATA="{\"walletAddress\":$CBOR,\"walletRewardAddresses\":[\"e1$PKH\"],\"walletStakeKeyHash\":\"$PKH\",\"collateralUtxo\":[\"000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\"],\"walletUnusedAddresses\":[],\"walletUsedAddresses\":[$CBOR]}"

PAYLOAD=$(curl -s 'https://api.geniusyield.co/yield-farming/rewards/claim' \
  -H "authorization: WalletStakeKeyHash $PKH" \
  -H 'content-type: application/json' \
  --data-raw "$DATA" \
  --compressed | jq .transactionPayload)

echo ""
echo "tx payload: $PAYLOAD"
echo ""

TX=$(cat << EOF
{
    "type": "Witnessed Tx BabbageEra",
    "description": "Ledger Cddl Format",
    "cborHex": $PAYLOAD
}
EOF
)

TX_TMP_FILE=$(mktemp)
echo "$TX" > $TX_TMP_FILE

cardano-cli transaction sign --mainnet --tx-file "$TX_TMP_FILE" --signing-key-file "$1" --out-file "$TX_TMP_FILE"
cardano-cli transaction view --tx-file "$TX_TMP_FILE"
echo ""

TID=$(cardano-cli transaction txid --tx-file "$TX_TMP_FILE")

cardano-cli transaction submit --socket-path "$2" --mainnet --tx-file "$TX_TMP_FILE"

rm $TX_TMP_FILE

echo "tx-id: $TID"
echo ""
