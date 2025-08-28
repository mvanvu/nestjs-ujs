#!/bin/bash
set -e

ROOT_PATH=$(pwd)

yarn docker:build && yarn docker:init && yarn build

# Deploy system microservice
echo "===== SYSTEM MICROSERVICE ====="
cd ${ROOT_PATH}/build/microservice/system
yarn deploy

# Deploy user microservice
echo "===== USER MICROSERVICE ====="
cd ${ROOT_PATH}/build/microservice/user
yarn deploy
echo "User microservice done"

# Deploy content microservice
echo "===== CONTENT MICROSERVICE ====="
cd ${ROOT_PATH}/build/microservice/content
yarn deploy
echo "Content microservice done"

# Deploy api gateway
echo "===== API GATEWAY ====="
cd ${ROOT_PATH}/build/api-gateway
yarn deploy