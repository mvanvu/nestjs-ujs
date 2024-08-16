import { Schema } from '@mvanvu/ujs';
import Mail from 'nodemailer/lib/mailer';

export class MessageInfoEntity {
   @(Schema.content().decorate())
   messageId: string;

   @(Schema.content().array().decorate())
   accepted: Array<string | Mail.Address>;

   @(Schema.content().array().decorate())
   rejected: Array<string | Mail.Address>;

   @(Schema.content().array().decorate())
   pending: Array<string | Mail.Address>;

   @(Schema.content().decorate())
   response: string;
}
