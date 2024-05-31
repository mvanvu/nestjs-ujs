import { Body, Controller, Delete, Get, HttpStatus, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   BaseController,
   EntityResponse,
   PaginationResponse,
   Permission,
} from '../../lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { serviceConfig } from '@config';
import { CreateRoleDto, RoleEntity, UpdateRoleDto } from '@lib/service/user';
const { name, permissions, patterns } = serviceConfig.get('user');

@ApiBearerAuth()
@ApiTags('Groups')
@Controller('groups')
export class GroupController extends BaseController {
   get userProxy(): BaseClientProxy {
      return this.createClientProxy(name);
   }

   @Permission({ key: permissions.group.read })
   @ApiPaginationResponse(RoleEntity, { summary: 'Get list pagination of groups' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<RoleEntity>> {
      return this.userProxy.send(patterns.groupCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.group.read })
   @ApiEntityResponse(RoleEntity, { summary: 'Get detail of groups' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(patterns.groupCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.group.create })
   @ApiEntityResponse(RoleEntity, { summary: 'Create a new group', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateRoleDto): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(patterns.groupCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Permission({ key: permissions.group.update })
   @Patch(':id')
   @ApiEntityResponse(RoleEntity, { summary: 'Update new group' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateRoleDto): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(patterns.groupCRUD, { data, meta: { params: { id }, CRUD: { method: 'write' } } });
   }

   @Permission({ key: permissions.group.delete })
   @ApiEntityResponse(RoleEntity, { summary: 'Delete a group' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RoleEntity>> {
      return this.userProxy.send(patterns.groupCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
