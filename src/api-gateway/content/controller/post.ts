import { ApiEntityResponse, ApiPaginationResponse, BaseClientProxy, Permission } from '@gateway/@library';
import { CRUDClient, EntityResult, PaginationResult, ParseMongoIdPipe } from '@shared-library';
import { serviceConfig } from '@metadata';
import { CreatePostDto, PostPaginationQueryDto, UpdatePostDto } from '@microservice/content/dto';
import { PostEntity } from '@microservice/content/entity';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, permissions, patterns } = serviceConfig.get('content');

@ApiBearerAuth()
@ApiTags('Contents')
@Controller('posts')
export class ContentPostController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get postCRUD(): CRUDClient {
      return this.proxy.createClient(name).createCRUD(patterns.postCRUD);
   }

   @Permission({ key: permissions.post.read, adminScope: true })
   @ApiPaginationResponse(PostEntity, { summary: 'Get list pagination of content posts' })
   @Get()
   paginate(@Query() query: PostPaginationQueryDto): Promise<PaginationResult<PostEntity>> {
      return this.postCRUD.paginate(query);
   }

   @Permission({ key: permissions.post.read, adminScope: true })
   @ApiEntityResponse(PostEntity, { summary: 'Get detail of the post' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResult<PostEntity>> {
      return this.postCRUD.read(id);
   }

   @Permission({ key: permissions.post.create, adminScope: true })
   @ApiEntityResponse(PostEntity, { summary: 'Create a new post', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreatePostDto): Promise<EntityResult<PostEntity>> {
      return this.postCRUD.create(data);
   }

   @Permission({ key: permissions.post.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(PostEntity, { summary: 'Update a post' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdatePostDto): Promise<EntityResult<PostEntity>> {
      return this.postCRUD.update(id, data);
   }

   @Permission({ key: permissions.post.delete, adminScope: true })
   @ApiEntityResponse(PostEntity, { summary: 'Delete a post' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResult<PostEntity>> {
      return this.postCRUD.delete(id);
   }
}
