FROM ubuntu:24.04
RUN apt-get update
RUN apt-get install -y gnupg curl openssl
RUN curl -fsSL https://pgp.mongodb.com/server-7.0.asc | \
    gpg -o /etc/apt/trusted.gpg.d/mongodb-server-7.0.gpg \
   --dearmor
RUN echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu jammy/mongodb-org/7.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-7.0.list
RUN apt-get update
RUN apt-get install -y mongodb-org
RUN ps --no-headers -o comm 1
RUN curl -sfS https://dotenvx.sh | sh

COPY ./.env /
COPY ./docker/mongodb/data /docker-entrypoint-initdb.d/db
COPY ./docker/mongodb/dump.sh /docker-entrypoint-initdb.d/
COPY ./docker/mongodb/init.sh /docker-entrypoint-initdb.d/

VOLUME /docker/mongodb/data

# Important
VOLUME /data/db

RUN openssl rand -base64 756 > /docker-entrypoint-initdb.d/mongodb-keyfile
RUN chmod 400 /docker-entrypoint-initdb.d/mongodb-keyfile

EXPOSE 27017
EXPOSE 37017
EXPOSE 37018

CMD ["mongod"]