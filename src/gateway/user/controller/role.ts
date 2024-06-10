import { Body, Controller, Delete, Get, HttpStatus, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   BaseController,
   EntityResponse,
   HttpCache,
   PaginationResponse,
   Permission,
} from '@gateway/lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { serviceConfig } from '@metadata';
import { CreateRoleDto, RoleEntity, UpdateRoleDto } from '@lib/service/user';
const { name, permissions, patterns } = serviceConfig.get('user');

@ApiBearerAuth()
@ApiTags('Roles')
@Controller('roles')
@HttpCache({ cacheRefKeys: /\/(users|groups)\// }) // Purge users and groups caching when the method is not GET
export class RoleController extends BaseController {
   get userProxy(): BaseClientProxy {
      return this.createClientProxy(name);
   }

   @Permission({ key: permissions.role.read, adminScope: true })
   @ApiPaginationResponse(RoleEntity, { summary: 'Get list pagination of roles' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<RoleEntity>> {
      return this.userProxy.send(patterns.roleCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.role.read, adminScope: true })
   @ApiEntityResponse(RoleEntity, { summary: 'Get detail of roles' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(patterns.roleCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.role.create, adminScope: true })
   @ApiEntityResponse(RoleEntity, { summary: 'Create a new role', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateRoleDto): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(patterns.roleCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Permission({ key: permissions.role.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(RoleEntity, { summary: 'Update new role' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateRoleDto): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(patterns.roleCRUD, { data, meta: { params: { id }, CRUD: { method: 'write' } } });
   }

   @Permission({ key: permissions.role.delete, adminScope: true })
   @ApiEntityResponse(RoleEntity, { summary: 'Delete a role' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(patterns.roleCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
