import { Inject, Injectable } from '@nestjs/common';
import { RestaurantService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib/common';
import { RestaurantEntity } from '@lib/microservice/order';
const patterns = serviceConfig.get('order.patterns');

@Injectable()
export class RestaurantController {
   @Inject(RestaurantService) readonly restaurantService: RestaurantService;

   @MessagePattern(patterns.restaurantCRUD)
   executeCRUD(): Promise<CRUDResult<RestaurantEntity>> {
      return this.restaurantService.createCRUDService().execute();
   }
}
