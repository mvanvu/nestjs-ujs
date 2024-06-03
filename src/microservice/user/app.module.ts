import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../lib';
import { UserModule } from './user.module';
import { serviceConfig } from '@metadata';

@Module({
   imports: [UserModule],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('user.name'));
   }
}
