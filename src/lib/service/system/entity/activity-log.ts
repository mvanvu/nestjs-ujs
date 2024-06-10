import { User, ActivityLog } from '.prisma/system';
import { DeviceOS } from '@lib/common/type/common';
import { Property } from '@lib/common/decorator';
import { BaseEntity } from '@lib/common/entity';

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
      this.message = entity.messagePattern.replace(/[^a-zA-Z0-9]+/g, '_').toUpperCase();
      this.dataInput = entity.dataInput?.['origin'] ?? null;
      this.dataResult = entity.dataResult?.['origin'] ?? null;
   }
}
