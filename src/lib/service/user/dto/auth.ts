import { IPickType, IProperty } from '@lib/common';

export class UserSignUpDto {
   @IProperty({
      validate: { is: 'string' },
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
      swagger: { description: 'The name of user', type: 'string', example: 'Rainy' },
   })
   name?: string;

   @IProperty({
      validate: { is: 'string' },
      optional: true,
      swagger: { description: 'The username of user', type: 'string', example: 'rainy.mi' },
   })
   username?: string;

   @IProperty({
      validate: { is: 'email' },
      swagger: { description: 'The email of user', type: 'string', example: 'rainy.mi@email.com' },
   })
   email: string;

   @IProperty({
      validate: [{ is: 'strongPassword', meta: { minLength: 8, noSpaces: true } }],
      swagger: { description: 'The password of user', type: 'string', example: 'MyStr0ngPassWord!' },
   })
   password: string;

   @IProperty({
      validate: { is: 'equals', meta: { equalsTo: 'password' } },
      swagger: { description: 'The confirm password', type: 'string', example: 'MyStr0ngPassWord!' },
   })
   password2: string;
}

export class UserSignInDto extends IPickType(UserSignUpDto, ['username', 'password']) {}

export class AuthTokenDto {
   @IProperty({
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      swagger: { description: 'The token to verify' },
   })
   token: string;
}
