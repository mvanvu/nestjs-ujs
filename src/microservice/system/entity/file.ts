import { BooleanSchema, EnumSchema, IDSchema, StringSchema, UIntSchema } from '@shared-library';
import { $Enums } from '.prisma/system';

export class FileEntity {
   @IDSchema()
   id: string;

   @StringSchema()
   name: string;

   @UIntSchema()
   size: number;

   @BooleanSchema()
   isPublic: boolean;

   @EnumSchema(Object.values($Enums.FileType))
   type: $Enums.FileType;

   @StringSchema()
   mime: string;

   @EnumSchema(Object.values($Enums.SorageProvider))
   provider: $Enums.SorageProvider;

   @StringSchema()
   providerId: string;

   @StringSchema({ format: 'url' })
   url: string;
}
