#!/bin/bash

nodeEnv=${1:-"dev"}
outPath="/db-dump/$nodeEnv"

if [[ "$nodeEnv" == "test" ]]; then
  dbSuffix="Test"
else
  dbSuffix=""
fi

# Dump System DB
mongodump --uri="$MONGODB_BASE_URL/SystemService$dbSuffix?authSource=admin" --out="$outPath"

# Dump User DB
mongodump --uri="$MONGODB_BASE_URL/UserService$dbSuffix?authSource=admin" --out="$outPath"

# Dump Storage DB
mongodump --uri="$MONGODB_BASE_URL/StorageService$dbSuffix?authSource=admin" --out="$outPath"