import { UserSignInDto } from '@microservice/user/dto';
import { HttpStatus } from '@nestjs/common';
import { spec } from 'pactum';
const url = '/users/signin';

export const signIn = () => {
   describe(url, () => {
      const dto: UserSignInDto = { username: 'rainy.mi', password: 'MyStr0ngPassWord!' };

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
         spec()
            .post(url)
            .withBody({ ...dto, password: dto.password + '?' })
            .expectStatus(HttpStatus.BAD_REQUEST));

      it('Should sign in successfully', () => spec().post(url).withBody(dto).expectStatus(HttpStatus.OK));
   });
};
