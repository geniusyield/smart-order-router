#!/bin/bash

echo "bot payment signing key file: $1"

VKEY_TMP_FILE=$(mktemp)
cardano-cli key verification-key --signing-key-file "$1" --verification-key-file "$VKEY_TMP_FILE"

PKH_TMP_FILE=$(mktemp)
cardano-cli address key-hash --payment-verification-key-file "$VKEY_TMP_FILE" --out-file "$PKH_TMP_FILE"
PKH=$(cat "$PKH_TMP_FILE")
echo "bot pubkey hash: $PKH"

rm "$VKEY_TMP_FILE"
rm "$PKH_TMP_FILE"

curl 'https://api.geniusyield.co/user/connect' \
  -H 'content-type: application/json' \
  --data-raw "{\"walletStakeKeyHash\":\"$PKH\",\"userType\":\"INDIVIDUAL\"}" \
  --compressed > /dev/null 2>&1

curl 'https://api.geniusyield.co/yield-farming/rewards' \
  -H "authorization: WalletStakeKeyHash $PKH" \
  -H 'content-type: application/json' \
  --compressed

echo ""
