import { Inject, Injectable } from '@nestjs/common';
import { MailerService } from '../provider';
import { EventPattern, MessagePattern } from '@nestjs/microservices';
import { serviceConfig } from '@metadata';
import { MessageInfoEntity, SendMailDto, SendTestMailDto } from '@lib/service/mailer';
const patterns = serviceConfig.get('mailer.patterns');

@Injectable()
export class MailerController {
   @Inject(MailerService) readonly mailerService: MailerService;

   @EventPattern(patterns.send)
   send(dto: SendMailDto): Promise<MessageInfoEntity> {
      return this.mailerService.send(dto);
   }

   @MessagePattern(patterns.sendTest)
   sendTest(dto: SendTestMailDto): Promise<MessageInfoEntity> {
      return this.mailerService.sendTest(dto);
   }
}
