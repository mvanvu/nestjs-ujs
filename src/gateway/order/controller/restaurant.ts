import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
   Permission,
} from '@gateway/lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { RestaurantEntity, UpdateRestaurantDto, CreateRestaurantDto } from '@lib/service/order';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, permissions, patterns } = serviceConfig.get('order');

@ApiBearerAuth()
@ApiTags('Order restaurant')
@Controller('order/restaurant')
export class OrderRestaurantController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get orderProxy(): BaseClientProxy {
      return this.proxy.create(name);
   }

   @Permission({ key: permissions.restaurant.read, adminScope: true })
   @ApiPaginationResponse(RestaurantEntity, { summary: 'Get list pagination of order restaurants' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<RestaurantEntity>> {
      return this.orderProxy.send(patterns.restaurantCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.restaurant.read, adminScope: true })
   @ApiEntityResponse(RestaurantEntity, { summary: 'Get detail of the restaurant' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RestaurantEntity>> {
      return this.orderProxy.send(patterns.restaurantCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Permission({ key: permissions.restaurant.create, adminScope: true })
   @ApiEntityResponse(RestaurantEntity, { summary: 'Create a new restaurant', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateRestaurantDto): Promise<EntityResponse<RestaurantEntity>> {
      return this.orderProxy.send(patterns.restaurantCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Permission({ key: permissions.restaurant.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(RestaurantEntity, { summary: 'Update a restaurant' })
   update(
      @Param('id', ParseMongoIdPipe) id: string,
      @Body() data: UpdateRestaurantDto,
   ): Promise<EntityResponse<RestaurantEntity>> {
      return this.orderProxy.send(patterns.restaurantCRUD, {
         data,
         meta: { params: { id }, CRUD: { method: 'write' } },
      });
   }

   @Permission({ key: permissions.restaurant.delete, adminScope: true })
   @ApiEntityResponse(RestaurantEntity, { summary: 'Delete a restaurant' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RestaurantEntity>> {
      return this.orderProxy.send(patterns.restaurantCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
