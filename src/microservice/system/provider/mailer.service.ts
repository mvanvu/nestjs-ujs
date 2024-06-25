import { Inject, Injectable } from '@nestjs/common';
import { SMTPTransporter } from './mailer-transporter';
import { SendMailDto } from '../dto';
import { MessageInfoEntity } from '../entity';
import { SystemService } from './system.service';
import { SystemConfigDto } from '@shared-library';

@Injectable()
export class MailerService {
   @Inject(SystemService) private readonly systemService: SystemService;

   getTransporter(mailerConfig: SystemConfigDto['mailer']): SMTPTransporter {
      // Todo, add some tranporter
      return new SMTPTransporter(mailerConfig);
   }

   async send(dto: SendMailDto): Promise<MessageInfoEntity | false> {
      const result = await this.systemService
         .getConfig()
         .then((systemConfig) => this.getTransporter(systemConfig.mailer).send(dto));

      return result ? new MessageInfoEntity(result) : false;
   }
}
