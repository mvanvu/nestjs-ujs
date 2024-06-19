import { Inject } from '@nestjs/common';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult, GroupEntity } from '@lib/common';
import { serviceConfig } from '@metadata';
import { GroupService } from '../provider';
const patterns = serviceConfig.get('user.patterns');
export class GroupController {
   @Inject(GroupService) readonly groupService: GroupService;

   @MessagePattern(patterns.groupCRUD)
   executeCRUD(): Promise<CRUDResult<GroupEntity>> {
      return this.groupService.createCRUDService().execute();
   }
}
