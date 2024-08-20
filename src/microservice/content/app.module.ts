import { Module } from '@nestjs/common';
import { createMetaProvider, createMicroserviceApp } from '../@library';
import { serviceConfig } from '@metadata';
import { CategoryService, PostService, PrismaService, TagService } from './provider';
import { CategoryController, PostController, TagController } from './controller';
@Module({
   controllers: [CategoryController, PostController, TagController],
   providers: [createMetaProvider(), PrismaService, CategoryService, PostService, TagService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('content.name'));
   }
}
