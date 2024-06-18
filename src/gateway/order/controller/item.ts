import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
} from '@gateway/lib';
import { CRUDClient, PaginationQueryDto, ParseMongoIdPipe } from '@lib';
import { ItemEntity, CreateItemDto, UpdateItemDto } from '@service/order';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, patterns } = serviceConfig.get('order');

@ApiBearerAuth()
@ApiTags('Orders')
@Controller('order/items')
export class OrderItemController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get itemCRUD(): CRUDClient {
      return this.proxy.createClient(name).createCRUD(patterns.itemCRUD);
   }

   @ApiPaginationResponse(ItemEntity, { summary: 'Get list pagination of order items' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<ItemEntity>> {
      return this.itemCRUD.paginate(query);
   }

   @ApiEntityResponse(ItemEntity, { summary: 'Get detail of the item' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<ItemEntity>> {
      return this.itemCRUD.read(id);
   }

   @ApiEntityResponse(ItemEntity, { summary: 'Create a new item', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateItemDto): Promise<EntityResponse<ItemEntity>> {
      return this.itemCRUD.create(data);
   }

   @Patch(':id')
   @ApiEntityResponse(ItemEntity, { summary: 'Update a item' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateItemDto): Promise<EntityResponse<ItemEntity>> {
      return this.itemCRUD.update(id, data);
   }

   @ApiEntityResponse(ItemEntity, { summary: 'Delete a item' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<ItemEntity>> {
      return this.itemCRUD.delete(id);
   }
}
