import * as nodemailer from 'nodemailer';
import { BaseTransporter } from './base';
import { SystemConfigDto } from '@lib/common';

export class SMTPTransporter extends BaseTransporter {
   constructor(config: SystemConfigDto['mailer']) {
      super(config);
      this.transporter = nodemailer.createTransport({
         // pool: true,
         host: config.smtp.host,
         port: config.smtp.port, // 587
         secure: config.smtp.port === 465, // Use true for port 465, false for all other ports
         auth: {
            user: config.smtp.user,
            pass: config.smtp.pass,
         },
      });
   }
}
