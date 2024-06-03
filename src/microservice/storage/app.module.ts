import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../lib';
import { StorageModule } from './storage.module';
import { serviceConfig } from '@metadata';
@Module({
   imports: [StorageModule],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('storage.name'));
   }
}
