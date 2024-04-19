import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { createMicroserviceApp } from './rpc.base';
import { UserModule } from '@service/user/user.module';
import { userService } from '@lib/constant/user';

@Module({
   imports: [ConfigModule.forRoot({ isGlobal: true, expandVariables: true }), UserModule],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, `${userService.proxy}Queue`);
   }
}
