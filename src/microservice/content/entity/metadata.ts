import { Property } from '@lib/decorator/property';
import { BaseEntity } from '@lib/entity/base';

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
