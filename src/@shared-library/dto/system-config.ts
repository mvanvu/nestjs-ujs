import { Schema } from '@mvanvu/ujs';
import { MailerTransporter } from '../type/common';

const transporters: MailerTransporter[] = ['SMTP'];

export class MailerSMTPTransporterDto {
   @(Schema.string().decorate())
   host: string;

   @(Schema.uint().decorate())
   port: number;

   @(Schema.content().decorate())
   user: string;

   @(Schema.string().minLength(4).decorate())
   pass: string;
}

export class MailerConfigDto {
   @(Schema.email().decorate())
   appMail: string;

   @(Schema.enum(transporters).decorate())
   transporter: MailerTransporter;

   @(Schema.classRef(MailerSMTPTransporterDto).optional().decorate())
   smtp?: MailerSMTPTransporterDto;
}

export class SystemConfigDto {
   @(Schema.classRef(MailerConfigDto).optional().decorate())
   mailer?: MailerConfigDto;

   @(Schema.uint(true).optional().decorate())
   removeActivityLogsAfterDays?: number;
}
