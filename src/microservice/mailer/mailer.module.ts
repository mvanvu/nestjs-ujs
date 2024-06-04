import { Module } from '@nestjs/common';
import { MailerController } from './controller';

@Module({
   controllers: [MailerController],
   providers: [],
})
export class MailerModule {}
