import { DeviceOS } from '@lib/type/common';
import { Property } from '@lib/decorator/property';

export class ActivityLogAuthor {
   @Property({ validate: { is: 'mongoId' }, swagger: { disable: true } })
   id: string;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disable: true } })
   name?: string;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disable: true } })
   username?: string;

   @Property({ optional: true, validate: { is: 'email' }, swagger: { disable: true } })
   email?: string;
}

export class ActivityLogDto {
   @Property({ validate: { is: 'boolean' }, swagger: { disable: true } })
   success: boolean;

   @Property({ validate: [{ is: 'string' }, { is: 'empty', not: true }], swagger: { disable: true } })
   messagePattern: string;

   @Property({
      optional: true,
      validate: { is: 'object' },
      swagger: { disable: true },
   })
   dataInput?: { origin: any };

   @Property({ optional: true, validate: { is: 'object' }, swagger: { disable: true } })
   dataResult?: { origin: any };

   @Property({ optional: true, validate: { is: ActivityLogAuthor }, swagger: { disable: true } })
   author?: ActivityLogAuthor;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disable: true } })
   ipAddress?: string;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disable: true } })
   userAgent?: string;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disable: true } })
   deviceType?: string;

   @Property({ optional: true, validate: { is: 'object' }, swagger: { disable: true } })
   deviceOS?: DeviceOS;
}
