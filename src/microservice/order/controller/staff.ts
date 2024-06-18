import { Inject, Injectable } from '@nestjs/common';
import { StaffService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib';
import { StaffEntity } from '@service/order';
const patterns = serviceConfig.get('order.patterns');

@Injectable()
export class StaffController {
   @Inject(StaffService) readonly staffService: StaffService;

   @MessagePattern(patterns.staffCRUD)
   executeCRUD(): Promise<CRUDResult<StaffEntity>> {
      return this.staffService.createCRUDService().execute();
   }
}
