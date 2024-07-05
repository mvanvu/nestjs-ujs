import { Prisma } from '.prisma/system';
import { BooleanSchema, JsonSchema, ClassSchema, StringSchema, UserRefEntity } from '@shared-library';

export class ActivityLogDto {
   @BooleanSchema()
   success: boolean;

   @StringSchema({ empty: false })
   messagePattern: string;

   @JsonSchema({ optional: true, swagger: false })
   dataInput?: { origin: any };

   @JsonSchema({ optional: true, swagger: false })
   dataResult?: { origin: any };

   @ClassSchema(UserRefEntity, { optional: true })
   author?: UserRefEntity;

   @StringSchema({ optional: true })
   ipAddress?: string;

   @StringSchema({ optional: true })
   userAgent?: string;

   @JsonSchema({ optional: true, swagger: false })
   detectResult?: Prisma.InputJsonObject;
}
