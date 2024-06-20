import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../@library';
import { serviceConfig } from '@metadata';
import { SystemController } from './controller';
import { SystemService, PrismaService } from './provider';
@Module({
   controllers: [SystemController],
   providers: [PrismaService, SystemService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('system.name'));
   }
}
