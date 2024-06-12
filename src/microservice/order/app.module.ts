import { Module } from '@nestjs/common';
import { createMicroserviceApp } from '../lib';
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
   providers: [PrismaService, CategoryService, ItemService, RestaurantService, StaffService, TableService],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('order.name'));
   }
}
