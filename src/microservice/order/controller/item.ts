import { Controller, Inject } from '@nestjs/common';
import { ItemService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@shared-library';
import { ItemEntity } from '../entity';
const patterns = serviceConfig.get('order.patterns');

@Controller()
export class ItemController {
   @Inject(ItemService) readonly itemService: ItemService;

   @MessagePattern(patterns.itemCRUD)
   executeCRUD(): Promise<CRUDResult<ItemEntity>> {
      return this.itemService.createCRUDService().execute();
   }
}
