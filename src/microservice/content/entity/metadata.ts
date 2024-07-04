import { IDSchema, ImageSchema, StringSchema } from '@shared-library';

export class MetadataEntity {
   @IDSchema({ optional: true })
   title?: string;

   @StringSchema({ optional: true })
   description?: string;

   @ImageSchema({ optional: true })
   image?: string;

   @StringSchema({ optional: true })
   author?: string;

   @StringSchema({ optional: true })
   robot?: string;

   @StringSchema({ optional: true })
   index?: string;
}
