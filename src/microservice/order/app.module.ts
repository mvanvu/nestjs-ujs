import { Module } from '@nestjs/common';
import { createMetaProvider, createMicroserviceApp } from '../@library';
import { serviceConfig } from '@metadata';
import { CategoryService, ItemService, PrismaService, RestaurantService, StaffService, TableService } from './provider';
import {
   CategoryController,
   ItemController,
   RestaurantController,
   StaffController,
   TableController,
} from './controller';
@Module({
   controllers: [CategoryController, ItemController, RestaurantController, StaffController, TableController],
   providers: [
      createMetaProvider(),
      PrismaService,
      CategoryService,
      ItemService,
      RestaurantService,
      StaffService,
      TableService,
   ],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('order.name'));
   }
}
