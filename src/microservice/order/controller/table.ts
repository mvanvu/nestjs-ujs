import { Inject, Injectable } from '@nestjs/common';
import { TableService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib/common';
import { TableEntity } from '@lib/service/order';
const patterns = serviceConfig.get('order.patterns');

@Injectable()
export class TableController {
   @Inject(TableService) readonly tableService: TableService;

   @MessagePattern(patterns.staffCRUD)
   executeCRUD(): Promise<CRUDResult<TableEntity>> {
      return this.tableService.createCRUDService().execute();
   }
}
