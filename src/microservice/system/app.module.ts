import { Module } from '@nestjs/common';
import { createMetaProvider, createMicroserviceApp } from '../@library';
import { serviceConfig } from '@metadata';
import { SystemController } from './controller';
import { SystemService, PrismaService, MailerService } from './provider';
@Module({
   controllers: [SystemController],
   providers: [createMetaProvider(), PrismaService, SystemService, MailerService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('system.name'));
   }
}
