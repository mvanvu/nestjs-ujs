import { Module } from '@nestjs/common';
import { GroupController, RoleController, UserController } from './controller';

@Module({
   controllers: [GroupController, RoleController, UserController],
})
export class UserModule {}
