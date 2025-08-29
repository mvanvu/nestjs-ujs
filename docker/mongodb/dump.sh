#!/bin/bash
set -e

nodeEnv=${1:-"dev"}
outPath="/docker-entrypoint-initdb.d/data"

# Must re-declare as it can't expand from .env file
MONGODB_BASE_URL="mongodb://${MONGODB_USERNAME}:${MONGODB_PASSWORD}@localhost:${MONGODB_PRIMARY_CONTAINER_PORT},localhost:${MONGODB_SECONDARY_CONTAINER_PORT}"

# Dump System DB
mongodump --uri="$MONGODB_BASE_URL/SystemService$dbSuffix?authSource=admin" --out="$outPath"

# Dump User DB
mongodump --uri="$MONGODB_BASE_URL/UserService$dbSuffix?authSource=admin" --out="$outPath"

# Dump Content DB
mongodump --uri="$MONGODB_BASE_URL/ContentService$dbSuffix?authSource=admin" --out="$outPath"