import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   HttpCache,
   PaginationResponse,
   Permission,
} from '@gateway/@library';
import { CRUDClient, PaginationQueryDto, ParseMongoIdPipe, RoleEntity } from '@shared-library';
import { serviceConfig } from '@metadata';
import { CreateRoleDto, UpdateRoleDto } from '@microservice/user/dto';
const { name, permissions, patterns } = serviceConfig.get('user');

@ApiBearerAuth()
@ApiTags('Roles')
@Controller('roles')
@HttpCache({ cacheRefKeys: /\/(users|groups)\// }) // Purge users and groups caching when the method is not GET
export class RoleController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get roleCRUD(): CRUDClient {
      return this.proxy.createClient(name).createCRUD(patterns.roleCRUD);
   }

   @Permission({ key: permissions.role.read, adminScope: true })
   @ApiPaginationResponse(RoleEntity, { summary: 'Get list pagination of roles' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<RoleEntity>> {
      return this.roleCRUD.paginate(query);
   }

   @Permission({ key: permissions.role.read, adminScope: true })
   @ApiEntityResponse(RoleEntity, { summary: 'Get detail of roles' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RoleEntity>> {
      return this.roleCRUD.read(id);
   }

   @Permission({ key: permissions.role.create, adminScope: true })
   @ApiEntityResponse(RoleEntity, { summary: 'Create a new role', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateRoleDto): Promise<EntityResponse<RoleEntity>> {
      return this.roleCRUD.create(data);
   }

   @Permission({ key: permissions.role.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(RoleEntity, { summary: 'Update new role' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateRoleDto): Promise<EntityResponse<RoleEntity>> {
      return this.roleCRUD.update(id, data);
   }

   @Permission({ key: permissions.role.delete, adminScope: true })
   @ApiEntityResponse(RoleEntity, { summary: 'Delete a role' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RoleEntity>> {
      return this.roleCRUD.delete(id);
   }
}
