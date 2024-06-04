import SMTPTransport from 'nodemailer/lib/smtp-transport';

export type TransporterList = 'smtp';

export type TransporterMessageInfo = SMTPTransport.SentMessageInfo;
