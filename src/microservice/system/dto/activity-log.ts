import { Prisma } from '.prisma/system';
import { Schema } from '@mvanvu/ujs';
import { UserRefEntity } from '@shared-library';

export class ActivityLogDto {
   @(Schema.boolean().decorate())
   success: boolean;

   @(Schema.content().decorate())
   messagePattern: string;

   @(Schema.object().optional().decorate())
   dataInput?: { origin: any };

   @(Schema.object().optional().decorate())
   dataResult?: { origin: any };

   @(Schema.classRef(UserRefEntity).optional().decorate())
   author?: UserRefEntity;

   @(Schema.ipv4().optional().decorate())
   ipAddress?: string;

   @(Schema.string().optional().decorate())
   userAgent?: string;

   @(Schema.object().optional().decorate())
   detectResult?: Prisma.InputJsonObject;
}
