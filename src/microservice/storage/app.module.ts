import { Module } from '@nestjs/common';
import { createMetaProvider, createMicroserviceApp } from '../@library';
import { serviceConfig } from '@metadata';
import { FileController } from './controller';
import { FileService, PrismaService } from './provider';
@Module({
   controllers: [FileController],
   providers: [createMetaProvider(), PrismaService, FileService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('storage.name'));
   }
}
