import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
   Permission,
} from '@gateway/lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { CategoryEntity, CreateCategoryDto, UpdateCategoryDto } from '@lib/service/content';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, permissions, patterns } = serviceConfig.get('content');

@ApiBearerAuth()
@ApiTags('Contents')
@Controller('content/categories')
export class ContentCategoryController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get contentProxy(): BaseClientProxy {
      return this.proxy.create(name);
   }

   @Permission({ key: permissions.category.read, adminScope: true })
   @ApiPaginationResponse(CategoryEntity, { summary: 'Get list pagination of content categories' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<CategoryEntity>> {
      return this.contentProxy.send(patterns.categoryCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.category.read, adminScope: true })
   @ApiEntityResponse(CategoryEntity, { summary: 'Get detail of the category' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<CategoryEntity>> {
      return this.contentProxy.send(patterns.categoryCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.category.create, adminScope: true })
   @ApiEntityResponse(CategoryEntity, { summary: 'Create a new category', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateCategoryDto): Promise<EntityResponse<CategoryEntity>> {
      return this.contentProxy.send(patterns.categoryCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Permission({ key: permissions.category.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(CategoryEntity, { summary: 'Update a category' })
   update(
      @Param('id', ParseMongoIdPipe) id: string,
      @Body() data: UpdateCategoryDto,
   ): Promise<EntityResponse<CategoryEntity>> {
      return this.contentProxy.send(patterns.categoryCRUD, {
         data,
         meta: { params: { id }, CRUD: { method: 'write' } },
      });
   }

   @Permission({ key: permissions.category.delete, adminScope: true })
   @ApiEntityResponse(CategoryEntity, { summary: 'Delete a category' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<CategoryEntity>> {
      return this.contentProxy.send(patterns.categoryCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
