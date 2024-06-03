import { $Enums } from '.prisma/user';
import { EntityProperty } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';

export class RoleGroupEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   name: string;

   @EntityProperty({ swagger: { type: [RoleGroupEntity] } })
   roles: RoleGroupEntity[];
}

export class ChildrenGroupEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   name: string;

   @EntityProperty({ swagger: { type: [RoleGroupEntity] } })
   roles: RoleGroupEntity[];
}

export class GroupEntity extends BaseEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   name: string;

   @EntityProperty({ swagger: { type: $Enums.AvailableStatus, enum: $Enums.AvailableStatus } })
   status: $Enums.AvailableStatus;

   @EntityProperty()
   createdAt?: Date;

   @EntityProperty()
   createdBy?: string;

   @EntityProperty()
   updatedAt?: Date;

   @EntityProperty()
   updatedBy?: string;

   @EntityProperty({ swagger: { type: [RoleGroupEntity] } })
   groups: RoleGroupEntity[];

   @EntityProperty({ swagger: { type: [RoleGroupEntity] } })
   roles: RoleGroupEntity[];
}
