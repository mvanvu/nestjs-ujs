import { ApiEntityResponse, ApiPaginationResponse, BaseClientProxy, HttpCache, Permission } from '@gateway/@library';
import { CRUDClient, EntityResult, PaginationQueryDto, PaginationResult, ParseMongoIdPipe } from '@shared-library';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import { TagEntity } from '@microservice/content/entity';
import { CreateTagDto, UpdateTagDto } from '@microservice/content/dto';

const { name, permissions, patterns } = serviceConfig.get('content');

@ApiBearerAuth()
@ApiTags('Contents')
@Controller('tags')
@HttpCache({ cacheRefKeys: /\/posts\// }) // Purge posts caching when the method is not GET
export class ContentTagController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get postCRUD(): CRUDClient {
      return this.proxy.createClient(name).createCRUD(patterns.tagCRUD);
   }

   @Permission({ key: permissions.tag.read, adminScope: true })
   @ApiPaginationResponse(TagEntity, { summary: 'Get list pagination of content tags' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResult<TagEntity>> {
      return this.postCRUD.paginate(query);
   }

   @Permission({ key: permissions.tag.read, adminScope: true })
   @ApiEntityResponse(TagEntity, { summary: 'Get detail of the tag' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResult<TagEntity>> {
      return this.postCRUD.read(id);
   }

   @Permission({ key: permissions.tag.create, adminScope: true })
   @ApiEntityResponse(TagEntity, { summary: 'Create a new tag', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateTagDto): Promise<EntityResult<TagEntity>> {
      return this.postCRUD.create(data);
   }

   @Permission({ key: permissions.tag.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(TagEntity, { summary: 'Update a tag' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateTagDto): Promise<EntityResult<TagEntity>> {
      return this.postCRUD.update(id, data);
   }

   @Permission({ key: permissions.tag.delete, adminScope: true })
   @ApiEntityResponse(TagEntity, { summary: 'Delete a tag' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResult<TagEntity>> {
      return this.postCRUD.delete(id);
   }
}
