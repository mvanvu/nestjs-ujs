import { Injectable } from '@nestjs/common';
import { SMTPTransporter } from '../transporter';
import { MessageInfoEntity, SendMailDto } from '@lib/service/mailer';
import { BaseService } from '@service/lib';

@Injectable()
export class MailerService extends BaseService {
   private static transporter: SMTPTransporter;

   get transporter(): SMTPTransporter {
      const mailerConfig = this.meta.get('headers.systemConfig.mailer');

      if (!MailerService.transporter) {
         switch (mailerConfig.transporter) {
            case 'SMTP':
               MailerService.transporter = new SMTPTransporter(mailerConfig);
               break;
         }
      }

      return MailerService.transporter;
   }

   async send(dto: SendMailDto): Promise<MessageInfoEntity | false> {
      const result = await this.transporter.send(dto);

      return result ? new MessageInfoEntity(result) : false;
   }
}
