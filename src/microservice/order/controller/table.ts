import { Inject, Injectable } from '@nestjs/common';
import { TableService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@shared-library';
import { TableEntity } from '../entity';
const patterns = serviceConfig.get('order.patterns');

@Injectable()
export class TableController {
   @Inject(TableService) readonly tableService: TableService;

   @MessagePattern(patterns.tableCRUD)
   executeCRUD(): Promise<CRUDResult<TableEntity>> {
      return this.tableService.createCRUDService().execute();
   }
}
