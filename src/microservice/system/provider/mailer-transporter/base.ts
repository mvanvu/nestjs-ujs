import * as nodemailer from 'nodemailer';
import { Transform } from '@mvanvu/ujs';
import { BaseEntity, TransporterMessageInfo } from '@shared-library';
import { MessageInfoEntity } from '@microservice/system/entity';
import { SendMailDto } from '@microservice/system/dto';

export class BaseTransporter {
   protected transporter: nodemailer.Transporter<TransporterMessageInfo>;

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

         return BaseEntity.bindToClass(info, MessageInfoEntity);
      } catch (e) {
         console.error('Send message failure:', e);

         return false;
      }
   }
}
