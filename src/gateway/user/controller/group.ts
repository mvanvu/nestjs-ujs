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
} from '@gateway/lib';
import { CRUDClient, PaginationQueryDto, ParseMongoIdPipe } from '@lib';
import { serviceConfig } from '@metadata';
import { GroupEntity, CreateGroupDto, UpdateGroupDto } from '@service/user';
const { name, permissions, patterns } = serviceConfig.get('user');

@ApiBearerAuth()
@ApiTags('Groups')
@Controller('groups')
@HttpCache({ cacheRefKeys: /\/(users|roles)\// }) // Purge users and roles caching when the method is not GET
export class GroupController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get groupCRUD(): CRUDClient {
      return this.proxy.createClient(name).createCRUD(patterns.groupCRUD);
   }

   @Permission({ key: permissions.group.read, adminScope: true })
   @ApiPaginationResponse(GroupEntity, { summary: 'Get list pagination of user groups' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<GroupEntity>> {
      return this.groupCRUD.paginate(query);
   }

   @Permission({ key: permissions.group.read, adminScope: true })
   @ApiEntityResponse(GroupEntity, { summary: 'Get detail of the user group' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<GroupEntity>> {
      return this.groupCRUD.read(id);
   }

   @Permission({ key: permissions.group.create, adminScope: true })
   @ApiEntityResponse(GroupEntity, { summary: 'Create a new user group', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateGroupDto): Promise<EntityResponse<GroupEntity>> {
      return this.groupCRUD.create(data);
   }

   @Permission({ key: permissions.group.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(GroupEntity, { summary: 'Update an user group' })
   update(
      @Param('id', ParseMongoIdPipe) id: string,
      @Body() data: UpdateGroupDto,
   ): Promise<EntityResponse<GroupEntity>> {
      return this.groupCRUD.update(id, data);
   }

   @Permission({ key: permissions.group.delete, adminScope: true })
   @ApiEntityResponse(GroupEntity, { summary: 'Delete an user group' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<GroupEntity>> {
      return this.groupCRUD.delete(id);
   }
}
