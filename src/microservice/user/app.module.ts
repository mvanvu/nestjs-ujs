import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../create-app';
import { UserModule } from '@/microservice/user/user.module';
import { userConfig } from '@/microservice/user/user.config';

@Module({
   imports: [UserModule],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, `${userConfig.proxy}Queue`);
   }
}
