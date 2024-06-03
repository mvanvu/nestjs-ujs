import { BaseEntity, IProperty } from '@lib/common';
import { $Enums } from '.prisma/storage';

export class FileEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty()
   name: string;

   @IProperty()
   size: number;

   @IProperty()
   isPublic: boolean;

   @IProperty({ swagger: { type: $Enums.FileType, enum: $Enums.FileType } })
   type: $Enums.FileType;

   @IProperty()
   mime: string;

   @IProperty({ swagger: { type: $Enums.Provider, enum: $Enums.Provider } })
   provider: $Enums.Provider;

   @IProperty()
   providerId: string;

   @IProperty()
   url: string;
}
