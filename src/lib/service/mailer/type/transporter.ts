import { MessageInfoEntity, SendMailDto } from '@lib/service/mailer';
import { Transporter } from 'nodemailer';
import SMTPTransport from 'nodemailer/lib/smtp-transport';

export type TransporterList = 'smtp';

export type TransporterMessageInfo = SMTPTransport.SentMessageInfo;

export interface TransporterInterface {
   readonly transporter: Transporter<TransporterMessageInfo>;
   send: (dto: SendMailDto) => Promise<MessageInfoEntity>;
}
