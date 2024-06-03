import { Module } from '@nestjs/common';
import { GroupController, RoleController, UserController } from './controller';
import { RoleService, UserService, PrismaService } from './provider';
import { GroupService } from './provider/group.service';

@Module({
   controllers: [UserController, RoleController, GroupController],
   providers: [UserService, PrismaService, RoleService, GroupService],
   exports: [UserService],
})
export class UserModule {}
