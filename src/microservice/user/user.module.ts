import { Module } from '@nestjs/common';
import { UserController } from './controller/user.controller';
import { RoleController } from './controller';
import { RoleService, UserService, PrismaService } from './provider';

@Module({
   controllers: [UserController, RoleController],
   providers: [UserService, PrismaService, RoleService],
   exports: [UserService],
})
export class UserModule {}
