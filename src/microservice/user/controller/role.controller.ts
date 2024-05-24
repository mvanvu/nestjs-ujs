import { Inject } from '@nestjs/common';
import { RoleService } from '../provider';
import { RoleEntity } from '@lib/service';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib/common';
import { serviceConfig } from '@config';
export class RoleController {
   @Inject(RoleService) readonly roleService: RoleService;

   @MessagePattern(serviceConfig.get('user.patterns.roleCRUD'))
   CRUD(): Promise<CRUDResult<RoleEntity>> {
      return this.roleService.roleCRUD().execute();
   }
}
