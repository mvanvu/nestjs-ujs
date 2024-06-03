import { $Enums } from '.prisma/user';
import { EntityProperty } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';

export class RoleEntity extends BaseEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   name: string;

   @EntityProperty({ swagger: { type: $Enums.AvailableStatus, enum: $Enums.AvailableStatus } })
   status: $Enums.AvailableStatus;

   @EntityProperty()
   createdAt?: Date;

   @EntityProperty()
   updatedAt?: Date;

   @EntityProperty()
   permissions: string[];
}
