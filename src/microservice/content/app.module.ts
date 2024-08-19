import { Module } from '@nestjs/common';
import { createMetaProvider, createMicroserviceApp } from '../@library';
import { serviceConfig } from '@metadata';
import { CategoryService, PrismaService } from './provider';
import { CategoryController } from './controller';
@Module({
   controllers: [CategoryController],
   providers: [createMetaProvider(), PrismaService, CategoryService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('content.name'));
   }
}
