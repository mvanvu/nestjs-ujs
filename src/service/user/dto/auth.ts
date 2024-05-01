import { IPickType, IProperty } from '@lib';

export class UserSignUpDto {
   @IProperty({
      swagger: { description: 'The name of user', example: 'Rainy' },
      validate: { is: 'string' },
      transform: { fromType: 'string', toType: 'trim' },
      optional: true,
   })
   name?: string;

   @IProperty({
      swagger: { description: 'The username of user', example: 'rainy.mi' },
      validate: { is: 'string' },
      optional: true,
   })
   username?: string;

   @IProperty({
      swagger: { description: 'The email of user', example: 'rainy.mi@email.com' },
      validate: { is: 'email' },
   })
   email?: string;

   @IProperty({
      swagger: { description: 'The password of user', example: 'MyStr0ngPassWord!' },
      validate: [{ is: 'strongPassword', meta: { minLength: 8, noSpaces: true } }],
   })
   password: string;

   @IProperty({
      swagger: { description: 'The confirm password', example: 'MyStr0ngPassWord!' },
      validate: { is: 'equals', meta: { equalsTo: 'password' } },
   })
   password2: string;
}

export class UserSignInDto extends IPickType(UserSignUpDto, ['username', 'email', 'password']) {}
