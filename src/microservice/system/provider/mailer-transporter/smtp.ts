import * as nodemailer from 'nodemailer';
import { BaseTransporter } from './base';
import { SystemConfigDto } from '@shared-library';

export class SMTPTransporter extends BaseTransporter {
   constructor(mailerConfig: SystemConfigDto['mailer']) {
      super();
      this.transporter = nodemailer.createTransport({
         // pool: true,
         host: mailerConfig.smtp.host,
         port: mailerConfig.smtp.port, // 587
         secure: mailerConfig.smtp.port === 465, // Use true for port 465, false for all other ports
         auth: {
            user: mailerConfig.smtp.user,
            pass: mailerConfig.smtp.pass,
         },
      });
   }
}
