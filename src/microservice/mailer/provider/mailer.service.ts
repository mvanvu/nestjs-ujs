import { serviceConfig } from '@metadata';
import { Injectable } from '@nestjs/common';
import { BaseService } from '@service/lib';
import { SMTPTransporter } from '../transporter';
import { MessageInfoEntity, SendMailDto, SendTestMailDto } from '@lib/service/mailer';

const mailerConfig = serviceConfig.get('mailer');

@Injectable()
export class MailerService extends BaseService {
   private static transporter: SMTPTransporter;

   get transporter(): SMTPTransporter {
      if (!MailerService.transporter) {
         switch (mailerConfig.transporter.default) {
            case 'smtp':
               MailerService.transporter = new SMTPTransporter();
               break;
         }
      }

      return MailerService.transporter;
   }

   async send(dto: SendMailDto): Promise<MessageInfoEntity> {
      return new MessageInfoEntity(await this.transporter.send(dto));
   }

   async sendTest(dto: SendTestMailDto): Promise<MessageInfoEntity> {
      return new MessageInfoEntity(await this.transporter.send(dto, true));
   }
}
