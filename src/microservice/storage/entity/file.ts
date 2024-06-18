import { BaseEntity, Property } from '@lib';
import { $Enums } from '.prisma/storage';

export class FileEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   name: string;

   @Property()
   size: number;

   @Property()
   isPublic: boolean;

   @Property({ swagger: { type: $Enums.FileType, enum: $Enums.FileType } })
   type: $Enums.FileType;

   @Property()
   mime: string;

   @Property({ swagger: { type: $Enums.Provider, enum: $Enums.Provider } })
   provider: $Enums.Provider;

   @Property()
   providerId: string;

   @Property()
   url: string;
}
