import { Inject } from '@nestjs/common';
import { RoleService } from '../provider';
import { RoleEntity, CreateRoleDto, UpdateRoleDto } from '@lib/service/user';
import { serviceConfig } from '@config';
import { MessagePattern, Payload } from '@nestjs/microservices';

export class RoleController {
   @Inject(RoleService) readonly roleService: RoleService;

   @MessagePattern(serviceConfig.get('user.patterns.readRole'))
   read(): Promise<RoleEntity> {
      return this.roleService.execute();
   }

   @MessagePattern(serviceConfig.get('user.patterns.writeRole'))
   write(@Payload() data: CreateRoleDto | UpdateRoleDto): Promise<RoleEntity> {
      return this.roleService.execute(data);
   }

   @MessagePattern(serviceConfig.get('user.patterns.deleteRole'))
   delete(): Promise<RoleEntity> {
      return this.roleService.execute(Promise);
   }
}
