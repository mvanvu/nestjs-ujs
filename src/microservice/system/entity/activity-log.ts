import { User, ActivityLog } from '.prisma/system';
import { DeviceOS } from '@lib/type/common';
import { Property } from '@lib/decorator';
import { BaseEntity } from '@lib/entity';

export class ActivityLogEntity extends BaseEntity<ActivityLog> {
   @Property()
   id: string;

   @Property()
   success: boolean;

   @Property()
   message: string;

   @Property()
   dataInput?: any;

   @Property()
   dataResult?: any;

   @Property()
   ipAddress?: string;

   @Property()
   deviceType?: string;

   @Property()
   deviceOS?: DeviceOS;

   @Property()
   userAgent?: string;

   @Property()
   author?: User;

   @Property()
   createdAt: Date;

   constructor(entity?: ActivityLog) {
      super(entity);

      if (entity) {
         this.dataInput = entity.dataInput?.['origin'] ?? null;
         this.dataResult = entity.dataResult?.['origin'] ?? null;
      }
   }
}
