import { Inject, Injectable } from '@nestjs/common';
import { RestaurantService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib';
import { RestaurantEntity } from '@service/order';
const patterns = serviceConfig.get('order.patterns');

@Injectable()
export class RestaurantController {
   @Inject(RestaurantService) readonly restaurantService: RestaurantService;

   @MessagePattern(patterns.restaurantCRUD)
   executeCRUD(): Promise<CRUDResult<RestaurantEntity>> {
      return this.restaurantService.createCRUDService().execute();
   }
}
