import { Inject, Injectable } from '@nestjs/common';
import { ItemService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib/common';
import { ItemEntity } from '@lib/service/order';
const patterns = serviceConfig.get('order.patterns');

@Injectable()
export class ItemController {
   @Inject(ItemService) readonly itemService: ItemService;

   @MessagePattern(patterns.itemCRUD)
   executeCRUD(): Promise<CRUDResult<ItemEntity>> {
      return this.itemService.createCRUDService().execute();
   }
}
