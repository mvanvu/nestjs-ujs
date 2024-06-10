import { BaseEntity } from '@lib/common/entity/base';
import { Property } from '@lib/common/decorator/property';
import { MailerTransporter } from '../type';

const transporters: MailerTransporter[] = ['SMTP'];

export class MailerSMTPTransporterDto {
   @Property({ validate: [{ is: 'string' }, { is: 'empty', not: true }] })
   host: string;

   @Property({ validate: [{ is: 'sInt' }] })
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

export class LanguageConfigDto {
   @Property({ validate: { is: 'boolean' }, swagger: { description: 'Enable multilingual mode, defaults to false' } })
   multilingual?: boolean;

   @Property({
      validate: { is: 'string' },
      swagger: { description: 'The default of language, defaults to en-GB', example: 'en-GB' },
   })
   defaultLanguage?: string;

   @Property({
      validate: { is: 'string', each: true },
      swagger: {
         description: 'List of allowed language ISO codes, defaults to * (* = all)',
         example: ['en-GB', 'en-US', 'vi-VN'],
      },
   })
   acceptLanguage?: string[];
}

export class SystemConfigDto extends BaseEntity {
   @Property({ optional: true, validate: { is: LanguageConfigDto }, swagger: { type: LanguageConfigDto } })
   language?: LanguageConfigDto;

   @Property({ optional: true, validate: { is: MailerConfigDto }, swagger: { type: MailerConfigDto } })
   mailer?: MailerConfigDto;
}
