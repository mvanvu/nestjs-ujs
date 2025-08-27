# NestJs UJS

A scaffold framework for initial projects built on NestJS, RabbitMQ, and MongoDB.

## Dependences

- [@mvanvu/ujs](https://github.com/mvanvu/ujs)
- [NestJs 10](https://nestjs.com/)
- [Docker](https://www.docker.com/)
- PM2
- Prisma
- Swagger
- RabbitMQ (for microservice)
- MongoDB
- argon2
- Nodemailer
- SWC
- ...

## Installation

### Download & install NODE (if not installed yet)

Download & install NODE at https://nodejs.org/en/download/

### Install Yarn (Latest) and PM2 (if not installed yet)

```
npm install -g yarn pm2
```

### Clone this repo

```
git clone https://github.com/mvanvu/nestjs-ujs.git

```

### Install dependences

```
cd nestjs-ujs
yarn

```

### Build docker images

```
yarn docker:build

```

### Start the application for dev

This takes the PM2 run the microservices and API gateway in watching mode

```
yarn start:dev
```

Note: Edit the hosts file on your computer to be able to connect to the MongoDB Replica Set. For dev only, no needed for deployment

```
127.0.0.1 nestjs-ujs-mongodb-primary
127.0.0.1 nestjs-ujs-mongodb-secondary
```

Browse the http://localhost:9001/api-docs an enjoy

### Deployment via Docker

Only one line script (useful for CI/CD), this will build/init/run the docker for each microservices and API gateway

```
yarn docker:deploy
```

## Destroy the resources (for dev)

Remove all Docker images, volumes, and PM2 processes.

```
yarn destroy:dev
```

## Default Microservices & API Gateway (See Swagger http://localhost:9000/api-docs)

### System microservice

- [POST] /api/v1/system/config (Save the system configuration)
- [GET] /api/v1/system/config (Get the system configuration)
- [GET] /api/v1/system/activity-logs (Get the activity logs for all of the api gateway)
- [POST] /api/v1/storage/upload (Upload files)
- [POST] /api/v1/mails/send-test (Send a testing email)

### User microservice

- CRUD /api/v1/groups
- CRUD /api/v1/roles
- CRUD /api/v1/users
- [POST] /api/v1/users/refresh-token
- [POST] /api/v1/users/verify-token
- [POST] /api/v1/users/signup
- [POST] /api/v1/users/activate
- [POST] /api/v1/users/send-reset-password
- [POST] /api/v1/users/reset-password
- [GET] /api/v1/users/me

### Content microservice

- CRUD /api/v1/categories
- CRUD /api/v1/tags
- CRUD /api/v1/posts
