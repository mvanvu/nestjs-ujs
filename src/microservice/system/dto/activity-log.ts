import { Prisma } from '.prisma/system';
import { Property } from '@shared-library';

export class ActivityLogAuthor {
   @Property({ validate: { is: 'mongoId' } })
   id: string;

   @Property({ optional: true, validate: { is: 'string' } })
   name?: string;

   @Property({ optional: true, validate: { is: 'string' } })
   username?: string;

   @Property({ optional: true, validate: { is: 'email' } })
   email?: string;
}

export class ActivityLogDto {
   @Property({ validate: { is: 'boolean' } })
   success: boolean;

   @Property({ validate: [{ is: 'string' }, { is: 'empty', not: true }] })
   messagePattern: string;

   @Property({
      optional: true,
      validate: { is: 'object' },
      swagger: { disabled: true },
   })
   dataInput?: { origin: any };

   @Property({ optional: true, validate: { is: 'object' } })
   dataResult?: { origin: any };

   @Property({ optional: true, validate: { is: ActivityLogAuthor } })
   author?: ActivityLogAuthor;

   @Property({ optional: true, validate: { is: 'string' } })
   ipAddress?: string;

   @Property({ optional: true, validate: { is: 'string' } })
   userAgent?: string;

   @Property({ optional: true, validate: { is: 'object' } })
   detectResult?: Prisma.InputJsonObject;
}
