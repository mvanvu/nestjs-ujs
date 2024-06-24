import { Controller, Inject } from '@nestjs/common';
import { RoleService } from '../provider';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult, RoleEntity } from '@shared-library';
import { serviceConfig } from '@metadata';
const patterns = serviceConfig.get('user.patterns');

@Controller()
export class RoleController {
   @Inject(RoleService) readonly roleService: RoleService;

   @MessagePattern(patterns.roleCRUD)
   executeCRUD(): Promise<CRUDResult<RoleEntity>> {
      return this.roleService.createCRUDService().execute();
   }
}
