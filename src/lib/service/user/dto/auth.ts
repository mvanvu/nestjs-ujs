import { IPickType, EntityProperty } from '@lib/common';
import { ApiProperty } from '@nestjs/swagger';

export class UserSignUpDto {
   @ApiProperty({ description: 'The name of user', type: 'string', example: 'Rainy' })
   @EntityProperty({ validate: { is: 'string' }, transform: { fromType: 'string', toType: 'trim' }, optional: true })
   name?: string;

   @ApiProperty({ description: 'The username of user', type: 'string', example: 'rainy.mi' })
   @EntityProperty({ validate: { is: 'string' }, optional: true })
   username?: string;

   @ApiProperty({ description: 'The email of user', type: 'string', example: 'rainy.mi@email.com' })
   @EntityProperty({ validate: { is: 'email' } })
   email: string;

   @ApiProperty({ description: 'The password of user', type: 'string', example: 'MyStr0ngPassWord!' })
   @EntityProperty({ validate: [{ is: 'strongPassword', meta: { minLength: 8, noSpaces: true } }] })
   password: string;

   @ApiProperty({ description: 'The confirm password', type: 'string', example: 'MyStr0ngPassWord!' })
   @EntityProperty({ validate: { is: 'equals', meta: { equalsTo: 'password' } } })
   password2: string;
}

export class UserSignInDto extends IPickType(UserSignUpDto, ['username', 'password']) {}
