# Base image: Ubuntu 24.04
FROM ubuntu:24.04

# Avoid interactive prompts during package installs
ENV DEBIAN_FRONTEND=noninteractive

# Update system and install required tools
RUN apt-get update && apt-get install -y \
    curl \
    gnupg \
    ca-certificates \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js (Latest LTS) + npm from NodeSource
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g npm@latest

# Install Yarn (Latest) and PM2
RUN npm install -g yarn pm2

# Set working directory
WORKDIR /usr/src/app

# Copy source code from host into container
COPY ./docker ./docker
COPY ./script ./script
COPY ./src ./src
COPY ./test ./test
COPY ./.env ./
COPY ./.env.test ./
COPY ./build.origin.json ./
COPY ./nest-cli.json ./
COPY ./package.json ./
COPY ./tsconfig.json ./

# Install dependences
RUN yarn install

# By default, mount host source code here
VOLUME ["/usr/src/app"]