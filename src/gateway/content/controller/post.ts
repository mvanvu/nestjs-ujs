import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
   Permission,
} from '@gateway/lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { PostEntity, CreatePostDto, UpdatePostDto } from '@lib/service/content';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, permissions, patterns } = serviceConfig.get('content');

@ApiBearerAuth()
@ApiTags('Contents')
@Controller('content/posts')
export class ContentPostController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get contentProxy(): BaseClientProxy {
      return this.proxy.create(name);
   }

   @Permission({ key: permissions.post.read, adminScope: true })
   @ApiPaginationResponse(PostEntity, { summary: 'Get list pagination of content posts' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<PostEntity>> {
      return this.contentProxy.send(patterns.postCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.post.read, adminScope: true })
   @ApiEntityResponse(PostEntity, { summary: 'Get detail of the post' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<PostEntity>> {
      return this.contentProxy.send(patterns.postCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.post.create, adminScope: true })
   @ApiEntityResponse(PostEntity, { summary: 'Create a new post', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreatePostDto): Promise<EntityResponse<PostEntity>> {
      return this.contentProxy.send(patterns.postCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Permission({ key: permissions.post.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(PostEntity, { summary: 'Update a post' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdatePostDto): Promise<EntityResponse<PostEntity>> {
      return this.contentProxy.send(patterns.postCRUD, {
         data,
         meta: { params: { id }, CRUD: { method: 'write' } },
      });
   }

   @Permission({ key: permissions.post.delete, adminScope: true })
   @ApiEntityResponse(PostEntity, { summary: 'Delete a post' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<PostEntity>> {
      return this.contentProxy.send(patterns.postCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
