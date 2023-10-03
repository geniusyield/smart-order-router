
# Orderbot runner using Maestro provider.
orderbot-maestro:
	cabal run geniusyield-orderbot-exe -- "run" \
                  ./config-files/atlas-config-maestro.json \
                  ./config-files/bot-config.json

# Orderbot runner using Blockfrost provider.
orderbot-blockfrost:
	cabal run geniusyield-orderbot-exe -- "run" \
                  ./config-files/atlas-config-blockfrost.json \
                  ./config-files/bot-config.json

# Orderbot runner using Blockfrost provider.
orderbot-kupo:
	cabal run geniusyield-orderbot-exe -- "run" \
                  ./config-files/atlas-config-kupo.json \
                  ./config-files/bot-config.json

# Run the complete quickcheck testing suite.
orderbot-tests:
	cabal run tests
