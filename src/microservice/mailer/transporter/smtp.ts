import * as nodemailer from 'nodemailer';
import { serviceConfig } from '@metadata';
import { BaseTransporter } from './base';

export class SMTPTransporter extends BaseTransporter {
   protected readonly transporter = nodemailer.createTransport(serviceConfig.get('mailer.transporter.smtp'));
   protected readonly testTransporter = nodemailer.createTransport(serviceConfig.get('mailer.test.smtp'));
}
