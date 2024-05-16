import { Inject } from '@nestjs/common';
import { RoleService } from '../provider';
import { RoleEntity, CreateRoleDto, UpdateRoleDto } from '@lib/service/user';
import { serviceConfig } from '@config';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib/common';
export class RoleController {
   @Inject(RoleService) readonly roleService: RoleService;

   @MessagePattern(serviceConfig.get('user.patterns.roleCRUD'))
   CRUD(): Promise<CRUDResult<RoleEntity>> {
      return this.roleService.executeCRUD(CreateRoleDto, UpdateRoleDto);
   }
}
