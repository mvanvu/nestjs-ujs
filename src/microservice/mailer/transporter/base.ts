import * as nodemailer from 'nodemailer';
import { Transform } from '@mvanvu/ujs';
import { MessageInfoEntity, SendMailDto, TransporterMessageInfo } from '@lib/service/mailer';

export abstract class BaseTransporter {
   protected abstract readonly transporter: nodemailer.Transporter<TransporterMessageInfo>;

   protected abstract readonly testTransporter: nodemailer.Transporter<TransporterMessageInfo>;

   async send(dto: SendMailDto, test?: boolean): Promise<MessageInfoEntity> {
      try {
         const info = await (test === true ? this.testTransporter : this.transporter).sendMail({
            from: dto.from, // Sender address
            to: dto.to, // List of receivers: bar@example.com, baz@example.com
            subject: dto.subject, // Subject line
            html: dto.body, // html body
            text: Transform.toStripTags(dto.body), // plain text body
         });

         console.log('Message sent: %s', info.messageId);

         return new MessageInfoEntity(info);
      } catch (e) {
         console.error('Send message failure:', e);
      }
   }
}
