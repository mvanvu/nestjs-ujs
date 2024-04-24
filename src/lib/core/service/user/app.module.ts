import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '@lib/core/service/base';
import { UserModule } from '@service/user/user.module';
import { userConfig } from '@service/user/user.config';

@Module({
   imports: [UserModule],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, `${userConfig.proxy}Queue`);
   }
}
