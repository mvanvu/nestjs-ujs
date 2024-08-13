import { ApiEntityResponse, ApiPaginationResponse, BaseClientProxy, Permission } from '@gateway/@library';
import { CRUDClient, EntityResult, PaginationQueryDto, PaginationResult, ParseTypePipe } from '@shared-library';
import { CreateCategoryDto, UpdateCategoryDto } from '@microservice/content/dto';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import { CategoryEntity } from '@microservice/content/entity';

const { name, permissions, patterns } = serviceConfig.get('content');

@ApiBearerAuth()
@ApiTags('Contents')
@Controller('content/categories')
export class ContentCategoryController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get categoryCRUD(): CRUDClient {
      return this.proxy.createClient(name).createCRUD(patterns.categoryCRUD);
   }

   @Permission({ key: permissions.category.read, adminScope: true })
   @ApiPaginationResponse(CategoryEntity, { summary: 'Get list pagination of content categories' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResult<CategoryEntity>> {
      return this.categoryCRUD.paginate(query);
   }

   @Permission({ key: permissions.category.read, adminScope: true })
   @ApiEntityResponse(CategoryEntity, { summary: 'Get detail of the category' })
   @Get(':id')
   read(@Param('id', ParseTypePipe('mongoId')) id: string): Promise<EntityResult<CategoryEntity>> {
      return this.categoryCRUD.read(id);
   }

   @Permission({ key: permissions.category.create, adminScope: true })
   @ApiEntityResponse(CategoryEntity, { summary: 'Create a new category', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateCategoryDto): Promise<EntityResult<CategoryEntity>> {
      return this.categoryCRUD.create(data);
   }

   @Permission({ key: permissions.category.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(CategoryEntity, { summary: 'Update a category' })
   update(
      @Param('id', ParseTypePipe('mongoId')) id: string,
      @Body() data: UpdateCategoryDto,
   ): Promise<EntityResult<CategoryEntity>> {
      return this.categoryCRUD.update(id, data);
   }

   @Permission({ key: permissions.category.delete, adminScope: true })
   @ApiEntityResponse(CategoryEntity, { summary: 'Delete a category' })
   @Delete(':id')
   delete(@Param('id', ParseTypePipe('mongoId')) id: string): Promise<EntityResult<CategoryEntity>> {
      return this.categoryCRUD.delete(id);
   }
}
