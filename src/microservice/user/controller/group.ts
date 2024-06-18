import { Inject } from '@nestjs/common';
import { GroupEntity } from '@service/user';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib';
import { serviceConfig } from '@metadata';
import { GroupService } from '../provider/group.service';
const patterns = serviceConfig.get('user.patterns');
export class GroupController {
   @Inject(GroupService) readonly groupService: GroupService;

   @MessagePattern(patterns.groupCRUD)
   executeCRUD(): Promise<CRUDResult<GroupEntity>> {
      return this.groupService.createCRUDService().execute();
   }
}
