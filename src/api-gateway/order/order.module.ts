import { Module } from '@nestjs/common';
import {
   OrderCategoryController,
   OrderItemController,
   OrderRestaurantController,
   OrderStaffController,
   OrderTableController,
} from './controller';

@Module({
   controllers: [
      OrderCategoryController,
      OrderItemController,
      OrderRestaurantController,
      OrderStaffController,
      OrderTableController,
   ],
})
export class OrderModule {}
