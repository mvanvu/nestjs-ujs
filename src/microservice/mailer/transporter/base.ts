import * as nodemailer from 'nodemailer';
import { Transform } from '@mvanvu/ujs';
import { MessageInfoEntity, SendMailDto, TransporterMessageInfo } from '@lib/service/mailer';
import { SystemConfigDto } from '@lib/service/system';

export class BaseTransporter {
   protected transporter: nodemailer.Transporter<TransporterMessageInfo>;

   constructor(protected readonly config: SystemConfigDto['mailer']) {}

   async send(dto: SendMailDto): Promise<MessageInfoEntity | false> {
      try {
         const info = await this.transporter.sendMail({
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

         return false;
      }
   }
}
