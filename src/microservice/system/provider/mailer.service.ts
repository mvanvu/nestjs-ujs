import { Inject, Injectable } from '@nestjs/common';
import { getSystemConfig, updateSystemConfig } from '@microservice/@library';
import { SMTPTransporter } from './mailer-transporter';
import { SendMailDto } from '../dto';
import { MessageInfoEntity } from '../entity';
import { Is } from '@mvanvu/ujs';
import { SystemService } from './system.service';

@Injectable()
export class MailerService {
   @Inject(SystemService) private readonly systemService: SystemService;

   async initMailerConfig() {
      const systemConfig = getSystemConfig('system');

      if (Is.emptyObject(systemConfig)) {
         updateSystemConfig('system', await this.systemService.getConfig());
      }
   }

   get transporter(): SMTPTransporter {
      // Todo, add some tranporter
      return new SMTPTransporter(getSystemConfig('system').mailer);
   }

   async send(dto: SendMailDto): Promise<MessageInfoEntity | false> {
      const result = await this.initMailerConfig().then(() => this.transporter.send(dto));

      return result ? new MessageInfoEntity(result) : false;
   }
}
