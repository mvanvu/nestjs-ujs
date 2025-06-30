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

## Installation

### Clone this repo

```
git clone https://github.com/mvanvu/nestjs-ujs.git

```

### Install dependences

```
cd nestjs-ujs
yarn

```

### Docker initialise

```
yarn docker:init

```

### Start the application for dev

```
yarn start:dev
```

Browse the http://localhost:9000/api-docs an enjoy
