import { BaseEntity, IProperty } from '@lib';
import { Provider, FileType } from '.prisma/storage';

export class FileEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty()
   name: string;

   @IProperty()
   size: number;

   @IProperty()
   isPublic: boolean;

   @IProperty()
   type: FileType;

   @IProperty()
   mime: string;

   @IProperty()
   provider: Provider;

   @IProperty()
   providerId: string;

   @IProperty()
   url: string;
}
