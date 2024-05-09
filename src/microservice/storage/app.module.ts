import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../create-app';
import { StorageModule } from '@/microservice/storage/storage.module';
import { storageConfig } from '@/microservice/storage/storage.config';

@Module({
   imports: [StorageModule],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, `${storageConfig.proxy}Queue`);
   }
}
