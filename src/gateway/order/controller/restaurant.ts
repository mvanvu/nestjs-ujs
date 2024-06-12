import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
   Permission,
} from '@gateway/lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { UserEntity } from '@lib/service';
import { RestaurantEntity, UpdateRestaurantDto, CreateRestaurantDto } from '@lib/service/order';
import { serviceConfig } from '@metadata';
import { Is } from '@mvanvu/ujs';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, permissions, patterns } = serviceConfig.get('order');

@ApiBearerAuth()
@ApiTags('Orders')
@Controller('order/restaurant')
export class OrderRestaurantController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get orderClient(): BaseClientProxy {
      return this.proxy.createClient(name);
   }

   @Permission({ key: permissions.restaurant.read, adminScope: true })
   @ApiPaginationResponse(RestaurantEntity, { summary: 'Get list pagination of order restaurants' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<RestaurantEntity>> {
      return this.orderClient.createCRUD(patterns.restaurantCRUD).paginate(query);
   }

   @Permission({ key: permissions.restaurant.read, adminScope: true })
   @ApiEntityResponse(RestaurantEntity, { summary: 'Get detail of the restaurant' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RestaurantEntity>> {
      return this.orderClient.createCRUD(patterns.restaurantCRUD).read(id);
   }

   private async validateUserData(data: CreateRestaurantDto | UpdateRestaurantDto, keyId: string) {
      if (!Is.string(data[keyId])) {
         return;
      }

      const user = await this.proxy
         .createClient(serviceConfig.get('user.name'))
         .createCRUD(serviceConfig.get('user.patterns.userCRUD'), { noEmitEvent: true })
         .read<UserEntity>(data.ownerId);
      data[keyId.replace('Id', '')] = UserEntity.toUserRefEntity(user);
      delete data[keyId];
   }

   @Permission({ key: permissions.restaurant.create, adminScope: true })
   @ApiEntityResponse(RestaurantEntity, { summary: 'Create a new restaurant', statusCode: HttpStatus.CREATED })
   @Post()
   async create(@Body() data: CreateRestaurantDto): Promise<EntityResponse<RestaurantEntity>> {
      return this.validateUserData(data, 'ownerId').then(() =>
         this.orderClient.createCRUD(patterns.restaurantCRUD).create(data),
      );
   }

   @Permission({ key: permissions.restaurant.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(RestaurantEntity, { summary: 'Update a restaurant' })
   async update(
      @Param('id', ParseMongoIdPipe) id: string,
      @Body() data: UpdateRestaurantDto,
   ): Promise<EntityResponse<RestaurantEntity>> {
      return this.validateUserData(data, 'ownerId').then(() =>
         this.orderClient.createCRUD(patterns.restaurantCRUD).update(id, data),
      );
   }

   @Permission({ key: permissions.restaurant.delete, adminScope: true })
   @ApiEntityResponse(RestaurantEntity, { summary: 'Delete a restaurant' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RestaurantEntity>> {
      return this.orderClient.createCRUD(patterns.restaurantCRUD).delete(id);
   }
}
