import { BaseEntity, EntityProperty } from '@lib/common';
import { $Enums } from '.prisma/storage';

export class FileEntity extends BaseEntity {
   @EntityProperty()
   id: string;

   @EntityProperty()
   name: string;

   @EntityProperty()
   size: number;

   @EntityProperty()
   isPublic: boolean;

   @EntityProperty({ swagger: { type: $Enums.FileType, enum: $Enums.FileType } })
   type: $Enums.FileType;

   @EntityProperty()
   mime: string;

   @EntityProperty({ swagger: { type: $Enums.Provider, enum: $Enums.Provider } })
   provider: $Enums.Provider;

   @EntityProperty()
   providerId: string;

   @EntityProperty()
   url: string;
}
