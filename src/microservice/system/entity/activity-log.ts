import { ActivityLog } from '.prisma/system';
import { BooleanSchema, JsonSchema, ClassSchema, StringSchema, UserRefEntity } from '@shared-library';
import { DetectResult } from 'node-device-detector';

export class ActivityLogEntity {
   @StringSchema()
   id: string;

   @BooleanSchema()
   success: boolean;

   @StringSchema()
   messagePattern: string;

   @JsonSchema({ optional: true })
   dataInput?: object;

   @JsonSchema({ optional: true })
   dataResult?: object;

   @StringSchema()
   ipAddress?: string;

   @StringSchema()
   deviceType?: string;

   @JsonSchema()
   detectResult?: DetectResult;

   @StringSchema()
   userAgent?: string;

   @ClassSchema(UserRefEntity)
   author?: UserRefEntity;

   @StringSchema({ format: 'date-time' })
   createdAt: Date;

   bind(entity: ActivityLog) {
      this.dataInput = entity.dataInput?.['origin'] ?? null;
      this.dataResult = entity.dataResult?.['origin'] ?? null;
   }
}
