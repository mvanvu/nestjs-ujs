import { Controller, Inject } from '@nestjs/common';
import { StaffService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@shared-library';
import { StaffEntity } from '../entity';
const patterns = serviceConfig.get('order.patterns');

@Controller()
export class StaffController {
   @Inject(StaffService) readonly staffService: StaffService;

   @MessagePattern(patterns.staffCRUD)
   executeCRUD(): Promise<CRUDResult<StaffEntity>> {
      return this.staffService.createCRUDService().execute();
   }
}
