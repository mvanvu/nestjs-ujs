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

```
yarn start:dev
```

Browse the http://localhost:9001/api-docs an enjoy

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
