import { EnumSchema, NumberSchema, ObjectSchema, StringSchema } from '@shared-library/decorator';
import { MailerTransporter } from '../type/common';

const transporters: MailerTransporter[] = ['SMTP'];

export class MailerSMTPTransporterDto {
   @StringSchema({ notEmpty: true })
   host: string;

   @NumberSchema({ min: 0 })
   port: number;

   @StringSchema({ notEmpty: true })
   user: string;

   @StringSchema({ notEmpty: true })
   pass: string;
}

export class MailerConfigDto {
   @StringSchema({ format: 'email' })
   appMail: string;

   @EnumSchema(transporters)
   transporter: MailerTransporter;

   @ObjectSchema(MailerSMTPTransporterDto, { optional: true })
   smtp?: MailerSMTPTransporterDto;
}

export class SystemConfigDto {
   @ObjectSchema(MailerConfigDto, { optional: true })
   mailer?: MailerConfigDto;

   @NumberSchema({ optional: true, integer: true, min: 1 })
   removeActivityLogsAfterDays?: number;
}
