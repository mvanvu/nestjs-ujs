import { Body, Controller, HttpStatus, Post } from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';
import { serviceConfig } from '@metadata';
import { MessageInfoEntity, SendMailDto } from '@lib/service/mailer';
import { ApiEntityResponse, BaseClientProxy, BaseController } from '@gateway/lib';

const { name, patterns } = serviceConfig.get('mailer');

@ApiTags('Mailer')
@Controller('mails')
export class MailController extends BaseController {
   get mailerProxy(): BaseClientProxy {
      return this.createClientProxy(name);
   }

   @Post('send-test')
   @ApiEntityResponse(MessageInfoEntity, { statusCode: HttpStatus.OK })
   sendTestMail(@Body() data: SendMailDto): Promise<MessageInfoEntity> {
      return this.mailerProxy.send(patterns.sendTest, { data });
   }
}
