import { Property } from '../decorator/property';
import { MailerTransporter } from '../type/common';

const transporters: MailerTransporter[] = ['SMTP'];

export class MailerSMTPTransporterDto {
   @Property({ validate: [{ is: 'string' }, { is: 'empty', not: true }] })
   host: string;

   @Property({ validate: [{ is: 'uInt' }] })
   port: number;

   @Property({ validate: [{ is: 'string' }, { is: 'empty', not: true }] })
   user: string;

   @Property({ validate: [{ is: 'string' }, { is: 'empty', not: true }] })
   pass: string;
}

export class MailerConfigDto {
   @Property({ validate: { is: 'email' } })
   appMail: string;

   @Property({
      validate: { is: 'inArray', meta: transporters },
      swagger: { description: 'The mailer transporter', enum: transporters },
   })
   transporter: MailerTransporter;

   @Property({
      optional: true,
      validate: { is: MailerSMTPTransporterDto },
      swagger: { description: 'Config for the SMTP transporter' },
   })
   smtp?: MailerSMTPTransporterDto;
}

export class SystemConfigDto {
   @Property({
      optional: true,
      validate: { is: MailerConfigDto },
      swagger: { type: MailerConfigDto, description: 'Mailer config' },
   })
   mailer?: MailerConfigDto;

   @Property({
      validate: [{ is: 'uInt' }, { is: 'min', meta: 1 }],
      optional: true,
      swagger: { description: 'The activity logs will be removed after this days number since the created date' },
   })
   removeActivityLogsAfterDays?: number;
}
