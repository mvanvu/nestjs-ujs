import { IsDTO, IsIn, IsNumber, IsString } from '../decorator/validator';
import { MailerTransporter } from '../type/common';

const transporters: MailerTransporter[] = ['SMTP'];

export class MailerSMTPTransporterDto {
   @IsString({ notEmpty: true })
   host: string;

   @IsNumber({ unsigned: true })
   port: number;

   @IsString({ notEmpty: true })
   user: string;

   @IsString({ notEmpty: true })
   pass: string;
}

export class MailerConfigDto {
   @IsString({ email: true })
   appMail: string;

   @IsIn(transporters, { swagger: { description: 'The mailer transporter', enum: transporters } })
   transporter: MailerTransporter;

   @IsDTO(MailerSMTPTransporterDto, { optional: true, swagger: { description: 'Config for the SMTP transporter' } })
   smtp?: MailerSMTPTransporterDto;
}

export class SystemConfigDto {
   @IsDTO(MailerConfigDto, { optional: true, swagger: { description: 'Mailer config' } })
   mailer?: MailerConfigDto;

   @IsNumber({
      optional: true,
      integer: true,
      unsigned: true,
      min: 1,
      swagger: { description: 'The activity logs will be removed after this days number since the created date' },
   })
   removeActivityLogsAfterDays?: number;
}
