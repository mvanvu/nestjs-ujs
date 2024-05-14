import { IPickType, Property } from '@lib';

export class UserSignUpDto {
   @Property({
      swagger: { description: 'The name of user', example: 'Rainy' },
      validate: { is: 'string' },
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
   })
   name?: string;

   @Property({
      swagger: { description: 'The username of user', example: 'rainy.mi' },
      validate: { is: 'string' },
      optional: true,
   })
   username?: string;

   @Property({
      swagger: { description: 'The email of user', example: 'rainy.mi@email.com' },
      validate: { is: 'email' },
   })
   email?: string;

   @Property({
      swagger: { description: 'The password of user', example: 'MyStr0ngPassWord!' },
      validate: [{ is: 'strongPassword', meta: { minLength: 8, noSpaces: true } }],
   })
   password: string;

   @Property({
      swagger: { description: 'The confirm password', example: 'MyStr0ngPassWord!' },
      validate: { is: 'equals', meta: { equalsTo: 'password' } },
   })
   password2: string;
}

export class UserSignInDto extends IPickType(UserSignUpDto, ['username', 'email', 'password']) {}
