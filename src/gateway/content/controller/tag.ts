import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
   Permission,
} from '@gateway/lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { TagEntity, CreateTagDto, UpdateTagDto } from '@lib/service/content';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, permissions, patterns } = serviceConfig.get('content');

@ApiBearerAuth()
@ApiTags('Contents')
@Controller('content/tags')
export class ContentTagController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get contentProxy(): BaseClientProxy {
      return this.proxy.create(name);
   }

   @Permission({ key: permissions.tag.read, adminScope: true })
   @ApiPaginationResponse(TagEntity, { summary: 'Get list pagination of content tags' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<TagEntity>> {
      return this.contentProxy.send(patterns.tagCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.tag.read, adminScope: true })
   @ApiEntityResponse(TagEntity, { summary: 'Get detail of the tag' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<TagEntity>> {
      return this.contentProxy.send(patterns.tagCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.tag.create, adminScope: true })
   @ApiEntityResponse(TagEntity, { summary: 'Create a new tag', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateTagDto): Promise<EntityResponse<TagEntity>> {
      return this.contentProxy.send(patterns.tagCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Permission({ key: permissions.tag.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(TagEntity, { summary: 'Update a tag' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateTagDto): Promise<EntityResponse<TagEntity>> {
      return this.contentProxy.send(patterns.tagCRUD, {
         data,
         meta: { params: { id }, CRUD: { method: 'write' } },
      });
   }

   @Permission({ key: permissions.tag.delete, adminScope: true })
   @ApiEntityResponse(TagEntity, { summary: 'Delete a tag' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<TagEntity>> {
      return this.contentProxy.send(patterns.tagCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
