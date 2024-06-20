import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
   Permission,
} from '@gateway/@library';
import { CRUDClient, PaginationQueryDto, ParseMongoIdPipe, UserRefEntity } from '@shared-library';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import { RestaurantEntity } from '@microservice/order/entity';
import { CreateRestaurantDto, UpdateRestaurantDto } from '@microservice/order/dto';

const { name, permissions, patterns } = serviceConfig.get('order');

@ApiBearerAuth()
@ApiTags('Orders')
@Controller('order/restaurant')
export class OrderRestaurantController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get clientProxy(): BaseClientProxy {
      return this.proxy.createClient(name);
   }

   get restaurantCRUD(): CRUDClient {
      return this.clientProxy.createCRUD(patterns.restaurantCRUD);
   }

   @Permission({ key: permissions.restaurant.read, adminScope: true })
   @ApiPaginationResponse(RestaurantEntity, { summary: 'Get list pagination of order restaurants' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<RestaurantEntity>> {
      return this.restaurantCRUD.paginate(query);
   }

   @Permission({ key: permissions.restaurant.read, adminScope: true })
   @ApiEntityResponse(RestaurantEntity, { summary: 'Get detail of the restaurant' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RestaurantEntity>> {
      return this.restaurantCRUD.read(id);
   }

   @Permission({ key: permissions.restaurant.create, adminScope: true })
   @ApiEntityResponse(RestaurantEntity, { summary: 'Create a new restaurant', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateRestaurantDto): Promise<EntityResponse<RestaurantEntity>> {
      return this.clientProxy.validateUserRef(data.ownerId, (userRef: UserRefEntity) => {
         delete data.ownerId;
         data.owner = userRef;

         return this.restaurantCRUD.create(data);
      });
   }

   @Permission({ key: permissions.restaurant.update, adminScope: true })
   @Patch(':id')
   @ApiEntityResponse(RestaurantEntity, { summary: 'Update a restaurant' })
   update(
      @Param('id', ParseMongoIdPipe) id: string,
      @Body() data: UpdateRestaurantDto,
   ): Promise<EntityResponse<RestaurantEntity>> {
      return this.clientProxy.validateUserRef(data.ownerId, (userRef?: UserRefEntity) => {
         if (userRef) {
            delete data.ownerId;
            data.owner = userRef;
         }

         return this.restaurantCRUD.update(id, data);
      });
   }

   @Permission({ key: permissions.restaurant.delete, adminScope: true })
   @ApiEntityResponse(RestaurantEntity, { summary: 'Delete a restaurant' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<RestaurantEntity>> {
      return this.restaurantCRUD.delete(id);
   }
}
