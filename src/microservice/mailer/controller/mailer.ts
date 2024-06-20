import { Inject, Injectable } from '@nestjs/common';
import { MailerService } from '../provider';
import { EventPattern } from '@nestjs/microservices';
import { serviceConfig } from '@metadata';
import { SendMailDto } from '../dto';
import { MessageInfoEntity } from '../entity';
const patterns = serviceConfig.get('mailer.patterns');

@Injectable()
export class MailerController {
   @Inject(MailerService) readonly mailerService: MailerService;

   @EventPattern(patterns.send)
   send(dto: SendMailDto): Promise<MessageInfoEntity | false> {
      return this.mailerService.send(dto);
   }
}
