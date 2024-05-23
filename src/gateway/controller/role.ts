import { Body, Controller, Delete, Get, HttpStatus, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import { BaseClientProxy, BaseController } from '../lib';
import {
   PaginationQueryDto,
   ParseMongoIdPipe,
   Permission,
   ApiResultResponse,
   Pagination,
   EntityResponse,
} from '@lib/common';
import { serviceConfig } from '@config';
import { CreateRoleDto, RoleEntity, UpdateRoleDto } from '@lib/service/user';
const roleCRUDPattern: string = serviceConfig.get('user.patterns.roleCRUD');

@ApiBearerAuth()
@ApiTags('Roles')
@Controller('roles')
export class RoleController extends BaseController {
   get userProxy(): BaseClientProxy {
      return this.createClientProxy(serviceConfig.get('user.name'));
   }

   @Permission({ key: serviceConfig.get('user.permissions.role.read') })
   @ApiResultResponse(RoleEntity, { summary: 'Get list pagination of roles', pagination: true })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<Pagination<RoleEntity>> {
      return this.userProxy.send(roleCRUDPattern, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Permission({ key: serviceConfig.get('user.permissions.role.read') })
   @ApiResultResponse(RoleEntity, { summary: 'Get detail of roles' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(roleCRUDPattern, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Permission({ key: serviceConfig.get('user.permissions.role.create') })
   @ApiResultResponse(RoleEntity, { summary: 'Create a new role', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateRoleDto): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(roleCRUDPattern, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Permission({ key: serviceConfig.get('user.permissions.role.update') })
   @Patch(':id')
   @ApiResultResponse(RoleEntity, { summary: 'Update new role' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateRoleDto): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(roleCRUDPattern, { data, meta: { params: { id }, CRUD: { method: 'write' } } });
   }

   @Permission({ key: serviceConfig.get('user.permissions.role.delete') })
   @ApiResultResponse(RoleEntity, { summary: 'Delete a role' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(roleCRUDPattern, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
