import { CreateUserDto } from '@microservice/user/dto';
import { HttpStatus } from '@nestjs/common';
import { spec, handler } from 'pactum';
import { ContainerTest } from '../container-test';
import { faker } from '@faker-js/faker';
import { availableStatuses } from '@shared-library';
const url = '/users';

ContainerTest.run('User CRUD', (c: ContainerTest) => {
   beforeAll(async () => {
      await c.signInWithRoot();
   });

   const password = `@Aa123${faker.internet.password({ length: 8 })}`;
   const createDTO: CreateUserDto = {
      email: faker.internet.email(),
      password,
      password2: password,
      username: faker.internet.userName(),
      avatarUrl: faker.image.avatar(),
      name: faker.internet.displayName(),
   };

   handler.addExpectHandler('userEntity', (ctx) => {
      expect(ctx.res.json['data'].username).toEqual(createDTO.username);
   });

   it('Should process the user CRUD successfully', async () => {
      // Create an user
      const user = await spec()
         .post(url)
         .withBearerToken('$S{rootAccessToken}')
         .withBody(createDTO)
         .expectStatus(HttpStatus.CREATED)
         .expect('userEntity')
         .returns('data');

      // Update the user status
      await spec()
         .patch(`${url}/${user.id}`)
         .withBearerToken('$S{rootAccessToken}')
         .withBody({ status: availableStatuses.Active })
         .expectStatus(HttpStatus.OK)
         .expectJsonLike('data.status', availableStatuses.Active);

      // Get detail of user
      await spec()
         .get(`${url}/${user.id}`)
         .withBearerToken('$S{rootAccessToken}')
         .withBody({ status: availableStatuses.Active })
         .expectStatus(HttpStatus.OK)
         .expect('userEntity');

      // Get list pagination of users
      await spec().get(url).withBearerToken('$S{rootAccessToken}').expectStatus(HttpStatus.OK);

      // Delete the user
      await spec()
         .delete(`${url}/${user.id}`)
         .withBearerToken('$S{rootAccessToken}')
         .expectStatus(HttpStatus.OK)
         .expect('userEntity');
   });
});
