import { Module } from '@nestjs/common';
import { createMetaProvider, createMicroserviceApp } from '../@library';
import { serviceConfig } from '@metadata';
import { GroupController, RoleController, UserController } from './controller';
import { PrismaService, RoleService, UserService, GroupService } from './provider';

@Module({
   controllers: [UserController, RoleController, GroupController],
   providers: [createMetaProvider(), UserService, PrismaService, RoleService, GroupService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('user.name'));
   }
}
