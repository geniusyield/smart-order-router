#!/bin/bash
echo $BOTC_CONFIG >> ~/config.json
geniusyield-orderbot-exe run ~/config.json
