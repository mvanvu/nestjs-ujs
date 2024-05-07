import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '@lib/core/service/base';
import { StorageModule } from '@service/storage/storage.module';
import { storageConfig } from '@service/storage/storage.config';

@Module({
   imports: [StorageModule],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, `${storageConfig.proxy}Queue`);
   }
}
