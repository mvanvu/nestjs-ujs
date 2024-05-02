import { Controller, Inject, RequestMethod } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import { RoleService } from '../provider';
import { IParam, IPayload, IQuery, IRoute, Permission } from '@lib/decorator';
import { rolePermissions, userConfig } from '../user.config';
import { PaginationQueryDto, ServiceExecuteResult } from '@lib';
import { SwaggerPaginationRoleEntity, RoleEntity } from '../entity';
import { CreateRoleDto, UpdateRoleDto } from '../dto';

@ApiTags('Roles')
@Controller('roles')
@ApiBearerAuth()
export class RoleController {
   @Inject(RoleService) readonly roleService: RoleService;

   @Permission({ key: rolePermissions.role.read })
   @IRoute({
      pattern: userConfig.patterns.rolePaginate,
      route: { method: RequestMethod.GET },
      swagger: {
         summary: 'Get list pagination of the roles',
         responseType: SwaggerPaginationRoleEntity,
      },
   })
   paginate(@IQuery() query: PaginationQueryDto): ServiceExecuteResult<RoleEntity> {
      return this.roleService.execute(userConfig.patterns.rolePaginate, { meta: { query } });
   }

   @Permission({ key: rolePermissions.role.read })
   @IRoute({
      pattern: userConfig.patterns.roleRead,
      route: { method: RequestMethod.GET, path: ':id' },
      swagger: { summary: 'Get the detail of the role', responseType: RoleEntity },
   })
   read(@IParam('id') id: string): ServiceExecuteResult<RoleEntity> {
      return this.roleService.execute(userConfig.patterns.roleRead, { meta: { params: { id } } });
   }

   @Permission({ key: rolePermissions.role.create })
   @IRoute({
      pattern: userConfig.patterns.roleCreate,
      route: { method: RequestMethod.POST },
      swagger: { summary: 'Create a new role', responseType: RoleEntity },
   })
   create(@IPayload() data: CreateRoleDto): ServiceExecuteResult<RoleEntity> {
      return this.roleService.execute(userConfig.patterns.roleCreate, { data });
   }

   @Permission({ key: rolePermissions.role.update })
   @IRoute({
      pattern: userConfig.patterns.roleUpdate,
      route: { method: RequestMethod.PATCH, path: ':id' },
      swagger: { summary: 'Update role', responseType: RoleEntity },
   })
   update(@IParam('id') id: string, @IPayload() data: UpdateRoleDto): ServiceExecuteResult<RoleEntity> {
      return this.roleService.execute(userConfig.patterns.roleUpdate, { data, meta: { params: { id } } });
   }

   @Permission({ key: rolePermissions.role.delete })
   @IRoute({
      pattern: userConfig.patterns.roleDelete,
      route: { method: RequestMethod.DELETE, path: ':id' },
      swagger: { summary: 'Delete a role', responseType: RoleEntity },
   })
   delete(@IParam('id') id: string): ServiceExecuteResult<RoleEntity> {
      return this.roleService.execute(userConfig.patterns.roleDelete, { meta: { params: { id } } });
   }
}
