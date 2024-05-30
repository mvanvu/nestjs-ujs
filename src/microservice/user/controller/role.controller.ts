import { Inject } from '@nestjs/common';
import { RoleService } from '../provider';
import { RoleEntity } from '@lib/service';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib/common';
import { serviceConfig } from '@config';
const patterns = serviceConfig.get('user.patterns');
export class RoleController {
   @Inject(RoleService) readonly roleService: RoleService;

   @MessagePattern(patterns.roleCRUD)
   executeCRUD(): Promise<CRUDResult<RoleEntity>> {
      return this.roleService.executeCRUD();
   }
}
