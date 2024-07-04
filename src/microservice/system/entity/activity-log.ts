import { ActivityLog } from '.prisma/system';
import {
   BooleanSchema,
   JsonSchema,
   ClassSchema,
   StringSchema,
   UserRefEntity,
   DateSchema,
   IDSchema,
} from '@shared-library';
import { DetectResult } from 'node-device-detector';

export class ActivityLogEntity {
   @IDSchema()
   id: string;

   @BooleanSchema()
   success: boolean;

   @StringSchema()
   messagePattern: string;

   @JsonSchema({ optional: true })
   dataInput?: object;

   @JsonSchema({ optional: true })
   dataResult?: object;

   @StringSchema({ optional: true, format: 'ipV4' })
   ipAddress?: string;

   @StringSchema({ optional: true })
   deviceType?: string;

   @JsonSchema({ optional: true })
   detectResult?: DetectResult;

   @StringSchema({ optional: true })
   userAgent?: string;

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @DateSchema()
   createdAt: Date;

   bind(entity: ActivityLog) {
      this.dataInput = entity.dataInput?.['origin'] ?? null;
      this.dataResult = entity.dataResult?.['origin'] ?? null;
   }
}
