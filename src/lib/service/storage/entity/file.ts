import { BaseEntity, EntityProperty } from '@lib/common';
import { Provider, FileType } from '.prisma/storage';

export class FileEntity extends BaseEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   name: string;

   @EntityProperty()
   size: number;

   @EntityProperty()
   isPublic: boolean;

   @EntityProperty()
   type: FileType;

   @EntityProperty()
   mime: string;

   @EntityProperty()
   provider: Provider;

   @EntityProperty()
   providerId: string;

   @EntityProperty()
   url: string;
}
