import { DeviceOS, Property } from '@shared-library';

export class ActivityLogAuthor {
   @Property({ validate: { is: 'mongoId' }, swagger: { disabled: true } })
   id: string;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disabled: true } })
   name?: string;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disabled: true } })
   username?: string;

   @Property({ optional: true, validate: { is: 'email' }, swagger: { disabled: true } })
   email?: string;
}

export class ActivityLogDto {
   @Property({ validate: { is: 'boolean' }, swagger: { disabled: true } })
   success: boolean;

   @Property({ validate: [{ is: 'string' }, { is: 'empty', not: true }], swagger: { disabled: true } })
   messagePattern: string;

   @Property({
      optional: true,
      validate: { is: 'object' },
      swagger: { disabled: true },
   })
   dataInput?: { origin: any };

   @Property({ optional: true, validate: { is: 'object' }, swagger: { disabled: true } })
   dataResult?: { origin: any };

   @Property({ optional: true, validate: { is: ActivityLogAuthor }, swagger: { disabled: true } })
   author?: ActivityLogAuthor;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disabled: true } })
   ipAddress?: string;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disabled: true } })
   userAgent?: string;

   @Property({ optional: true, validate: { is: 'string' }, swagger: { disabled: true } })
   deviceType?: string;

   @Property({ optional: true, validate: { is: 'object' }, swagger: { disabled: true } })
   deviceOS?: DeviceOS;
}
