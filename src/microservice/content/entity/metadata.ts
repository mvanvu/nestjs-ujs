import { Property, BaseEntity } from '@shared-library';

export class MetadataEntity extends BaseEntity {
   @Property()
   title?: string;

   @Property()
   description?: string;

   @Property()
   image?: string;

   @Property()
   author?: string;

   @Property()
   robot?: string;

   @Property()
   index?: string;
}
