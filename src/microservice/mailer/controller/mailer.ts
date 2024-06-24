import { Controller, Inject } from '@nestjs/common';
import { MailerService } from '../provider';
import { EventPattern } from '@nestjs/microservices';
import { serviceConfig } from '@metadata';
import { SendMailDto } from '../dto';
import { MessageInfoEntity } from '../entity';
import { SystemConfigDto } from '@shared-library';
import { updateSystemConfig } from '@microservice/@library';
const patterns = serviceConfig.get('mailer.patterns');

@Controller()
export class MailerController {
   @Inject(MailerService) readonly mailerService: MailerService;

   @EventPattern(patterns.send)
   send(dto: SendMailDto): Promise<MessageInfoEntity | false> {
      return this.mailerService.send(dto);
   }

   @EventPattern(patterns.storeMailerConfig)
   updateSystemConfig(data: SystemConfigDto): void {
      return updateSystemConfig('mailer', data.mailer || {});
   }
}
