import { HttpStatus } from '@nestjs/common';
import { spec } from 'pactum';
import { ContainerTest } from 'test/container-test';
const url = '/users/signin';

ContainerTest.run('Auth sign-in', (c: ContainerTest) => {
   it('Should throw if DTO is not valid', () =>
      spec()
         .post(url)
         .withBody({ username: 123 })
         .expectStatus(HttpStatus.BAD_REQUEST)
         .expectJsonLike({
            error: {
               username: [{ code: 'IS_STRING' }],
               password: [{ code: 'IS_STRONG_PASSWORD' }],
            },
         }));

   it('Should throw if wrong username or password', () =>
      spec().post(url).withBody({ username: 'fakeUA', password: 'fakePwd' }).expectStatus(HttpStatus.BAD_REQUEST));

   it('Should sign in successfully', () => c.signInWithRoot());
});
