import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../lib';
import { serviceConfig } from '@metadata';
import { FileController } from './controller';
import { FileService, PrismaService } from './provider';
@Module({
   controllers: [FileController],
   providers: [PrismaService, FileService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('storage.name'));
   }
}
