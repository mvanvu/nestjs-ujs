import { Injectable } from '@nestjs/common';
import { SMTPTransporter } from '../transporter';
import { BaseService, getSystemConfig } from '@microservice/@library';
import { SendMailDto } from '../dto';
import { MessageInfoEntity } from '../entity';

@Injectable()
export class MailerService extends BaseService {
   get transporter(): SMTPTransporter {
      // Todo, add some tranporter
      return new SMTPTransporter(getSystemConfig('mailer'));
   }

   async send(dto: SendMailDto): Promise<MessageInfoEntity | false> {
      const result = await this.transporter.send(dto);

      return result ? new MessageInfoEntity(result) : false;
   }
}
