import { StringSchema } from '@shared-library';

export class MetadataEntity {
   @StringSchema({ optional: true })
   title?: string;

   @StringSchema({ optional: true })
   description?: string;

   @StringSchema({ optional: true, format: 'url' })
   image?: string;

   @StringSchema({ optional: true })
   author?: string;

   @StringSchema({ optional: true })
   robot?: string;

   @StringSchema({ optional: true })
   index?: string;
}
