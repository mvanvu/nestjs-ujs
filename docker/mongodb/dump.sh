#!/bin/bash
set -e

nodeEnv=${1:-"dev"}
outPath="/docker/mongodb/data"

# Must re-declare as it can't expand from .env file
MONGODB_BASE_URL="mongodb://${MONGODB_USERNAME}:${MONGODB_PASSWORD}@localhost:${MONGODB_PRIMARY_PORT},localhost:${MONGODB_SECONDARY_PORT}"

# Dump System DB
mongodump --uri="$MONGODB_BASE_URL/SystemService$dbSuffix?authSource=admin" --out="$outPath"

# Dump User DB
mongodump --uri="$MONGODB_BASE_URL/UserService$dbSuffix?authSource=admin" --out="$outPath"

# Dump Storage DB
mongodump --uri="$MONGODB_BASE_URL/StorageService$dbSuffix?authSource=admin" --out="$outPath"