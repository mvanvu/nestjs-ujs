import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../@library';
import { serviceConfig } from '@metadata';
import { PrismaService } from './provider';
@Module({
   providers: [PrismaService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('content.name'));
   }
}
