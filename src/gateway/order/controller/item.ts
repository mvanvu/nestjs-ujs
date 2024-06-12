import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
} from '@gateway/lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { ItemEntity, CreateItemDto, UpdateItemDto } from '@lib/service/order';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, patterns } = serviceConfig.get('order');

@ApiBearerAuth()
@ApiTags('Order items')
@Controller('order/items')
export class OrderItemController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get orderProxy(): BaseClientProxy {
      return this.proxy.create(name);
   }

   @ApiPaginationResponse(ItemEntity, { summary: 'Get list pagination of order items' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<ItemEntity>> {
      return this.orderProxy.send(patterns.itemCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @ApiEntityResponse(ItemEntity, { summary: 'Get detail of the item' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<ItemEntity>> {
      return this.orderProxy.send(patterns.itemCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @ApiEntityResponse(ItemEntity, { summary: 'Create a new item', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateItemDto): Promise<EntityResponse<ItemEntity>> {
      return this.orderProxy.send(patterns.itemCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Patch(':id')
   @ApiEntityResponse(ItemEntity, { summary: 'Update a item' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateItemDto): Promise<EntityResponse<ItemEntity>> {
      return this.orderProxy.send(patterns.itemCRUD, {
         data,
         meta: { params: { id }, CRUD: { method: 'write' } },
      });
   }

   @ApiEntityResponse(ItemEntity, { summary: 'Delete a item' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<ItemEntity>> {
      return this.orderProxy.send(patterns.itemCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
