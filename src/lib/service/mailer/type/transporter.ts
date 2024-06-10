import SMTPPool from 'nodemailer/lib/smtp-pool';
import SMTPTransport from 'nodemailer/lib/smtp-transport';

export type TransporterList = 'smtp';

export type TransporterMessageInfo = SMTPTransport.SentMessageInfo | SMTPPool.SentMessageInfo;
