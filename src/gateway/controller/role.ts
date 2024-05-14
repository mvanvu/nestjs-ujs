import { Body, Controller, Delete, Get, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import { BaseController } from '../lib';
import { PaginationQueryDto, Permission } from '@lib';
import { serviceConfig } from '@config';
import { CreateRoleDto, RoleEntity, UpdateRoleDto } from '@lib/service/user';

@ApiTags('Roles')
@Controller('roles')
@ApiBearerAuth()
export class RoleController extends BaseController {
   readonly userProxy = this.createClientProxy(serviceConfig.get('user.proxy'));

   @Permission({ key: serviceConfig.get('user.permissions.role.read') })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<RoleEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.readRole'), { meta: { query } });
   }

   @Permission({ key: serviceConfig.get('user.permissions.role.read') })
   @Get(':id')
   read(@Param('id') id: string): Promise<RoleEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.readRole'), { meta: { params: { id } } });
   }

   @Permission({ key: serviceConfig.get('user.permissions.role.create') })
   @Post()
   create(@Body() data: CreateRoleDto): Promise<RoleEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.writeRole'), { data });
   }

   @Permission({ key: serviceConfig.get('user.permissions.role.update') })
   @Patch(':id')
   update(@Param('id') id: string, @Body() data: UpdateRoleDto): Promise<RoleEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.writeRole'), { data, meta: { params: { id } } });
   }

   @Permission({ key: serviceConfig.get('user.permissions.role.delete') })
   @Delete(':id')
   delete(@Param('id') id: string): Promise<RoleEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.deleteRole'), { meta: { params: { id } } });
   }
}
