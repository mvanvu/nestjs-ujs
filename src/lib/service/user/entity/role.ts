import { $Enums } from '.prisma/user';
import { IProperty } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';

export class RoleEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty()
   name: string;

   @IProperty({ swagger: { type: $Enums.AvailableStatus, enum: $Enums.AvailableStatus } })
   status: $Enums.AvailableStatus;

   @IProperty()
   createdAt?: Date;

   @IProperty()
   updatedAt?: Date;

   @IProperty()
   permissions: string[];
}
