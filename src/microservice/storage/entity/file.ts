import { BooleanSchema, EnumSchema, IDSchema, StringSchema, UIntSchema } from '@shared-library';
import { $Enums } from '.prisma/storage';

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

   @EnumSchema(Object.values($Enums.Provider))
   provider: $Enums.Provider;

   @StringSchema()
   providerId: string;

   @StringSchema()
   url: string;
}
