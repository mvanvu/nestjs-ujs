import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../lib';
import { serviceConfig } from '@metadata';
import { MailerController } from './controller';
import { MailerService } from './provider';
@Module({
   controllers: [MailerController],
   providers: [MailerService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('mailer.name'));
   }
}
