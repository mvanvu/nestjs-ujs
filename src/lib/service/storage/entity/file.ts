import { BaseEntity, Property } from '@lib';
import { Provider, FileType } from '.prisma/storage';

export class FileEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   name: string;

   @Property()
   size: number;

   @Property()
   isPublic: boolean;

   @Property()
   type: FileType;

   @Property()
   mime: string;

   @Property()
   provider: Provider;

   @Property()
   providerId: string;

   @Property()
   url: string;
}
