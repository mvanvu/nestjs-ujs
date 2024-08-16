import { Schema } from '@mvanvu/ujs';

export class SendMailDto {
   @(Schema.string().optional().decorate())
   from?: string;

   @(Schema.email().array().decorate())
   to: string[];

   @(Schema.content().decorate())
   subject: string;

   @(Schema.raw().decorate())
   body: string;
}

export class SendTestMailDto {
   @(Schema.email().decorate())
   email: string;

   @(Schema.content().eg('Test send email').decorate())
   subject: string;

   @(Schema.raw().eg('Send test mail successfully, this email for testing purpose').decorate())
   body: string;
}
