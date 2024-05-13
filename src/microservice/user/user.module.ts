import { Module } from '@nestjs/common';
import { RoleController, UserController } from './controller';
import { RoleService, UserService, PrismaService } from './provider';

@Module({
   controllers: [UserController, RoleController],
   providers: [UserService, PrismaService, RoleService],
   exports: [UserService],
})
export class UserModule {}
