import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
} from '@gateway/lib';
import { CRUDClient, ParseMongoIdPipe, ThrowException, User, UserEntity } from '@lib/common';
import {
   TableEntity,
   CreateTableDto,
   UpdateTableDto,
   OrderPaginationQueryDto,
   RestaurantEntity,
} from '@lib/microservice/order';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, patterns, permissions } = serviceConfig.get('order');

@ApiBearerAuth()
@ApiTags('Orders')
@Controller('order/tables')
export class OrderTableController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get tableCRUD(): CRUDClient {
      return this.proxy.createClient(name).createCRUD(patterns.tableCRUD);
   }

   @ApiPaginationResponse(TableEntity, { summary: 'Get list pagination of the order tables' })
   @Get()
   paginate(
      @Query() query: OrderPaginationQueryDto,
      @User() user: UserEntity,
   ): Promise<PaginationResponse<TableEntity>> {
      if (!user.authorise(permissions.restaurant.read)) {
         query.ownerId = user.id;
      }

      return this.tableCRUD.paginate(query);
   }

   @ApiEntityResponse(TableEntity, { summary: 'Get detail of the table' })
   @Get(':id')
   async read(@Param('id', ParseMongoIdPipe) id: string, @User() user: UserEntity): Promise<TableEntity> {
      const table = await this.tableCRUD.read<TableEntity>(id);

      if (!user.authorise(permissions.restaurant.read) && table.restaurant.owner.id !== user.id) {
         ThrowException(`Access denied. You don't have permission to access this resource`, HttpStatus.FORBIDDEN);
      }

      return table;
   }

   @ApiEntityResponse(TableEntity, { summary: 'Create a new table', statusCode: HttpStatus.CREATED })
   @Post()
   async create(@Body() data: CreateTableDto, @User() user: UserEntity): Promise<TableEntity> {
      const canCreate = user.authorise(permissions.restaurant.create);

      // Find all restaurant of the current owner user
      const { data: restaurants } = await this.proxy
         .createClient('order')
         .createCRUD(patterns.restaurantCRUD)
         .paginate<PaginationResponse<RestaurantEntity>>({ ownerId: user.id, limit: 1000 });

      // No restaurants found, throw 400
      if (!restaurants.length) {
         ThrowException(`The restaurant ID can't be empty`, HttpStatus.BAD_REQUEST);
      }

      // Have a reastaurant ID but it isn't your and you don't have access to create, throw 403
      if (!canCreate && !restaurants.find(({ id }) => id === data.restaurantId)) {
         ThrowException(`Access denied. You don't have permission to access this resource`, HttpStatus.FORBIDDEN);
      }

      return await this.tableCRUD.create(data);
   }

   @Patch(':id')
   @ApiEntityResponse(TableEntity, { summary: 'Update a table' })
   async update(
      @Param('id', ParseMongoIdPipe) id: string,
      @Body() data: UpdateTableDto,
      @User() user: UserEntity,
   ): Promise<EntityResponse<TableEntity>> {
      if (data.restaurantId && !user.authorise(permissions.restaurant.update)) {
         // Find the restaurant of the current owner user
         const restaurant = await this.proxy
            .createClient('order')
            .createCRUD(patterns.restaurantCRUD)
            .read<RestaurantEntity>(data.restaurantId);

         // This restaurant isn't your, throw 403
         if (restaurant.owner.id !== user.id) {
            ThrowException(`Access denied. You don't have permission to access this resource`, HttpStatus.FORBIDDEN);
         }
      }

      return await this.tableCRUD.update(id, data);
   }

   @ApiEntityResponse(TableEntity, { summary: 'Delete a table' })
   @Delete(':id')
   async delete(@Param('id', ParseMongoIdPipe) id: string, @User() user: UserEntity): Promise<TableEntity> {
      // Find the restaurant of the current owner user
      const restaurant = await this.proxy
         .createClient('order')
         .createCRUD(patterns.restaurantCRUD)
         .read<RestaurantEntity>(id);

      // This restaurant isn't your, throw 403
      if (restaurant.owner.id !== user.id && !user.authorise(permissions.restaurant.delete)) {
         ThrowException(`Access denied. You don't have permission to access this resource`, HttpStatus.FORBIDDEN);
      }

      return await this.tableCRUD.delete(id);
   }
}
