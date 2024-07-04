import { Prisma } from '.prisma/system';
import { BooleanSchema, JsonSchema, ClassSchema, StringSchema, IDSchema } from '@shared-library';

export class ActivityLogAuthor {
   @IDSchema()
   id: string;

   @StringSchema({ optional: true })
   name?: string;

   @StringSchema({ optional: true })
   username?: string;

   @IDSchema({ optional: true })
   email?: string;
}

export class ActivityLogDto {
   @BooleanSchema()
   success: boolean;

   @StringSchema({ empty: false })
   messagePattern: string;

   @JsonSchema({ optional: true, swagger: false })
   dataInput?: { origin: any };

   @JsonSchema({ optional: true, swagger: false })
   dataResult?: { origin: any };

   @ClassSchema(ActivityLogAuthor, { optional: true })
   author?: ActivityLogAuthor;

   @StringSchema({ optional: true })
   ipAddress?: string;

   @StringSchema({ optional: true })
   userAgent?: string;

   @JsonSchema({ optional: true, swagger: false })
   detectResult?: Prisma.InputJsonObject;
}
