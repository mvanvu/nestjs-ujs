#!/bin/bash
set -e

# Create a shared network
NETWORK_NAME=$APP_NAME-network
docker network inspect $NETWORK_NAME >/dev/null 2>&1 || docker network create $NETWORK_NAME

echo ">> Building images..."

# Build images
docker build -f ./docker/mongodb/Dockerfile -t $APP_NAME-mongodb .