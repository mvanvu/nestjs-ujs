import { ActivityLog } from '.prisma/system';
import { Schema } from '@mvanvu/ujs';
import { UserRefEntity } from '@shared-library';
import { DetectResult } from 'node-device-detector';

export class ActivityLogEntity {
   @(Schema.mongoId().decorate())
   id: string;

   @(Schema.boolean().decorate())
   success: boolean;

   @(Schema.content().decorate())
   messagePattern: string;

   @(Schema.object().optional().decorate())
   dataInput?: object;

   @(Schema.object().optional().decorate())
   dataResult?: object;

   @(Schema.ipv4().optional().decorate())
   ipAddress?: string;

   @(Schema.string().optional().decorate())
   deviceType?: string;

   @(Schema.object().optional().decorate())
   detectResult?: DetectResult;

   @(Schema.string().optional().decorate())
   userAgent?: string;

   @(Schema.classRef(UserRefEntity).optional().decorate())
   author?: UserRefEntity;

   @(Schema.dateTime().decorate())
   createdAt: Date;

   bind(entity: ActivityLog) {
      this.dataInput = entity.dataInput?.['origin'] ?? null;
      this.dataResult = entity.dataResult?.['origin'] ?? null;
   }
}
