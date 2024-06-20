import { Inject, Injectable } from '@nestjs/common';
import { RestaurantService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@shared-library';
import { RestaurantEntity } from '../entity';
const patterns = serviceConfig.get('order.patterns');

@Injectable()
export class RestaurantController {
   @Inject(RestaurantService) readonly restaurantService: RestaurantService;

   @MessagePattern(patterns.restaurantCRUD)
   executeCRUD(): Promise<CRUDResult<RestaurantEntity>> {
      return this.restaurantService.createCRUDService().execute();
   }
}
