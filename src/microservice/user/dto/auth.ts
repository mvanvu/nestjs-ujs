import { ClassRefSchema, Schema } from '@mvanvu/ujs';

export class UserSignUpDto {
   @(Schema.content().optional().eg('Rainy').decorate())
   name?: string;

   @(Schema.content().optional().eg('rainy.mi').decorate())
   username?: string;

   @(Schema.email().eg('rainy.mi@email.com').decorate())
   email: string;

   @(Schema.password().eg('MyStr0ngPassWord').decorate())
   password: string;

   @(Schema.password().eg('MyStr0ngPassWord').decorate())
   password2: string;
}

export class UserSignInDto extends ClassRefSchema.Pick(UserSignUpDto, ['username', 'password']) {}

export class AuthTokenDto {
   @(Schema.jwt().decorate())
   token: string;
}

export class SendResetPasswordCodeDto extends ClassRefSchema.Pick(UserSignUpDto, ['email']) {}

export class VerifyAccountDto {
   @(Schema.regex(/^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/).decorate())
   code: string;
}

export class ResetPasswordDto extends ClassRefSchema.Pick(UserSignUpDto, ['password', 'password2']) {
   @(Schema.regex(/^[0-9a-fA-F]{24}:[a-zA-Z0-9-]+$/).decorate())
   code: string;
}
