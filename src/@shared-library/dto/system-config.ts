import { EnumSchema, NumberSchema, ClassSchema, StringSchema } from '@shared-library/decorator';
import { MailerTransporter } from '../type/common';

const transporters: MailerTransporter[] = ['SMTP'];

export class MailerSMTPTransporterDto {
   @StringSchema({ empty: false })
   host: string;

   @NumberSchema({ min: 0 })
   port: number;

   @StringSchema({ empty: false })
   user: string;

   @StringSchema({ empty: false })
   pass: string;
}

export class MailerConfigDto {
   @StringSchema({ format: 'email' })
   appMail: string;

   @EnumSchema(transporters)
   transporter: MailerTransporter;

   @ClassSchema(MailerSMTPTransporterDto, { optional: true })
   smtp?: MailerSMTPTransporterDto;
}

export class SystemConfigDto {
   @ClassSchema(MailerConfigDto, { optional: true })
   mailer?: MailerConfigDto;

   @NumberSchema({ optional: true, integer: true, min: 1 })
   removeActivityLogsAfterDays?: number;
}
