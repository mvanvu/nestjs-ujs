#!/bin/bash
set -e

REPLICA_FILE="/docker-entrypoint-initdb.d/init-replica-set.js"

if [ -f "${REPLICA_FILE}" ]; then
    echo "✅ MongoDB is initialised."
else
    echo "🚀 Starting to init replica and data, waiting for 10 seconds..."
    sleep 10

    cat <<EOF > ${REPLICA_FILE}
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
            { _id: 0, host: process.env.APP_NAME + '-mongodb-primary:' + process.env.MONGODB_PRIMARY_CONTAINER_PORT, priority: 2 },
            { _id: 1, host: process.env.APP_NAME + '-mongodb-secondary:' + process.env.MONGODB_SECONDARY_CONTAINER_PORT, priority: 1 },
        ],
    });

    rs.status();
EOF

    chmod +x ${REPLICA_FILE}

    mongosh --port ${MONGODB_PRIMARY_CONTAINER_PORT} < ${REPLICA_FILE}

    # Must re-declare as it can't expand from .env file
    MONGODB_BASE_URL="mongodb://${MONGODB_USERNAME}:${MONGODB_PASSWORD}@localhost:${MONGODB_PRIMARY_CONTAINER_PORT},localhost:${MONGODB_SECONDARY_CONTAINER_PORT}"

    # Init data
    mongorestore --uri ${MONGODB_BASE_URL} --authenticationDatabase admin /docker-entrypoint-initdb.d/db
fi