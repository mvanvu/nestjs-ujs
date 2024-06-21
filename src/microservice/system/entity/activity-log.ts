import { User, ActivityLog } from '.prisma/system';
import { BaseEntity, Property } from '@shared-library';
import { DetectResult } from 'node-device-detector';

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
   device?: DetectResult;

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
