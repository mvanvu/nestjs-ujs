import { $Enums } from '.prisma/user';
import { IProperty } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';

export class RoleGroupEntity {
   @IProperty()
   id: string;

   @IProperty()
   name: string;

   @IProperty()
   permissions: string[];
}

export class ChildrenGroupEntity {
   @IProperty()
   id: string;

   @IProperty()
   name: string;

   @IProperty({ swagger: { type: [RoleGroupEntity] } })
   roles: RoleGroupEntity[];
}

export class GroupEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty()
   name: string;

   @IProperty({ swagger: { type: $Enums.AvailableStatus, enum: $Enums.AvailableStatus } })
   status: $Enums.AvailableStatus;

   @IProperty()
   createdAt?: Date;

   @IProperty()
   createdBy?: string;

   @IProperty()
   updatedAt?: Date;

   @IProperty()
   updatedBy?: string;

   @IProperty({ swagger: { type: [ChildrenGroupEntity] } })
   groups: ChildrenGroupEntity[];

   @IProperty({ swagger: { type: [RoleGroupEntity] } })
   roles: RoleGroupEntity[];
}
