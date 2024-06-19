import { Module } from '@nestjs/common';
import { MailerProvider } from './provider';

@Module({
   providers: [MailerProvider],
})
export class MailerModule {}
