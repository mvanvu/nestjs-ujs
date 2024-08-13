import { Schema } from '@mvanvu/ujs';
import { ApiProperty } from '@nestjs/swagger';
import { IPickType } from '@shared-library';

export class UserSignUpDto {
   @ApiProperty({ description: 'The name of user', example: 'Rainy' })
   @Schema.content().optional().decorate()
   name?: string;

   @ApiProperty({ description: 'The username of user', example: 'rainy.mi' })
   @Schema.content().optional().decorate()
   username?: string;

   @ApiProperty({ description: 'The email of user', example: 'rainy.mi@email.com' })
   @Schema.email().decorate()
   email: string;

   @ApiProperty({ description: 'The password of user', example: 'MyStr0ngPassWord!' })
   @Schema.password().decorate()
   password: string;

   @ApiProperty({ description: 'The password confirmation', example: 'MyStr0ngPassWord!' })
   @Schema.password().decorate()
   password2: string;
}

export class UserSignInDto extends IPickType(UserSignUpDto, ['username', 'password']) {}

export class AuthTokenDto {
   @Schema.jwt().decorate()
   token: string;
}

export class SendResetPasswordCodeDto extends IPickType(UserSignUpDto, ['email']) {}

export class VerifyAccountDto {
   @Schema.regex(/^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/).decorate()
   code: string;
}

export class ResetPasswordDto extends IPickType(UserSignUpDto, ['password', 'password2']) {
   @Schema.regex(/^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/).decorate()
   code: string;
}
