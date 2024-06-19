import { Injectable } from '@nestjs/common';
import { SMTPTransporter } from '../transporter';
import { MessageInfoEntity, SendMailDto } from '@lib/microservice/mailer';
import { BaseService } from '@microservice/lib';

@Injectable()
export class MailerService extends BaseService {
   private static transporter: SMTPTransporter;

   get transporter(): SMTPTransporter {
      const mailerConfig = this.meta.get('headers.systemConfig.mailer');

      // Todo, add some tranporter
      return new SMTPTransporter(mailerConfig);
   }

   async send(dto: SendMailDto): Promise<MessageInfoEntity | false> {
      const result = await this.transporter.send(dto);

      return result ? new MessageInfoEntity(result) : false;
   }
}
