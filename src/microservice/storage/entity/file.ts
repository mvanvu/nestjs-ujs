import { BooleanSchema, EnumSchema, NumberSchema, StringSchema } from '@shared-library';
import { $Enums } from '.prisma/storage';

export class FileEntity {
   @StringSchema()
   id: string;

   @StringSchema()
   name: string;

   @NumberSchema({ min: 0, integer: true })
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
