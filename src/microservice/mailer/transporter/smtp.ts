import * as nodemailer from 'nodemailer';
import { BaseTransporter } from './base';

export class SMTPTransporter extends BaseTransporter {
   protected readonly transporter = nodemailer.createTransport({
      pool: true,
      host: this.config.smtp.host,
      port: this.config.smtp.port, // 587
      secure: this.config.smtp.port === 465, // Use true for port 465, false for all other ports
      auth: {
         user: this.config.smtp.user,
         pass: this.config.smtp.pass,
      },
   });
}
