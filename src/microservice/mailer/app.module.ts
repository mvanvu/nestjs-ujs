import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../lib';
import { MailerModule } from './mailer.module';
import { serviceConfig } from '@metadata';
@Module({
   imports: [MailerModule],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('mailer.name'));
   }
}
