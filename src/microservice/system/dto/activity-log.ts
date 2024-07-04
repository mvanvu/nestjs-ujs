import { Prisma } from '.prisma/system';
import {
   BooleanSchema,
   JsonSchema,
   ClassSchema,
   StringSchema,
   IDSchema,
   NameSchema,
   EmailSchema,
} from '@shared-library';

export class ActivityLogAuthor {
   @IDSchema()
   id: string;

   @NameSchema({ optional: true })
   name?: string;

   @NameSchema({ optional: true })
   username?: string;

   @EmailSchema({ optional: true })
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

   @StringSchema({ optional: true, format: 'ipV4' })
   ipAddress?: string;

   @StringSchema({ optional: true })
   userAgent?: string;

   @JsonSchema({ optional: true, swagger: false })
   detectResult?: Prisma.InputJsonObject;
}
