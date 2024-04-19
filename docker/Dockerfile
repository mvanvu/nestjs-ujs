FROM ubuntu:22.04
RUN apt-get update
RUN apt-get install -y gnupg curl openssl
RUN curl -fsSL https://pgp.mongodb.com/server-7.0.asc | \
    gpg -o /etc/apt/trusted.gpg.d/mongodb-server-7.0.gpg \
   --dearmor
RUN echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu jammy/mongodb-org/7.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-7.0.list
RUN apt-get update
RUN apt-get install -y mongodb-org
RUN ps --no-headers -o comm 1

ENV MONGODB_USERNAME=admin
ENV MONGODB_PASSWORD=pass

VOLUME /data/db

RUN openssl rand -base64 756 > /mongodb-keyfile
RUN chmod 400 /mongodb-keyfile
RUN cat <<EOF > /init-replica-set.js
    sleep(10000);
    use admin;
    rs.initiate();
    db.createUser({
        user: process.env.MONGODB_USERNAME,
        pwd: process.env.MONGODB_PASSWORD,
        roles: [
            {
                role: 'root',
                db: 'admin',
            },
        ],
    });
    db.auth(process.env.MONGODB_USERNAME, process.env.MONGODB_PASSWORD);
    rs.reconfig({
        _id: 'rs0',
        members: [
            { _id: 0, host: 'localhost:' + process.env.MONGODB_PRIMARY_PORT, priority: 3 },
            { _id: 1, host: 'localhost:' + process.env.MONGODB_SECONDARY1_PORT, priority: 2 },
            { _id: 2, host: 'localhost:' + + process.env.MONGODB_SECONDARY2_PORT, priority: 1 },
        ],
    });
EOF
RUN chmod +x /init-replica-set.js

EXPOSE 27017

CMD ["mongod"]