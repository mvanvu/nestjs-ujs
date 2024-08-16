import { $Enums } from '.prisma/system';
import { Schema } from '@mvanvu/ujs';

export class FileEntity {
   @(Schema.mongoId().decorate())
   id: string;

   @(Schema.content().decorate())
   name: string;

   @(Schema.uint().decorate())
   size: number;

   @(Schema.boolean().decorate())
   isPublic: boolean;

   @(Schema.enum(Object.values($Enums.FileType)).decorate())
   type: $Enums.FileType;

   @(Schema.content().decorate())
   mime: string;

   @(Schema.enum(Object.values($Enums.SorageProvider)).decorate())
   provider: $Enums.SorageProvider;

   @(Schema.content().decorate())
   providerId: string;

   @(Schema.imageUri().decorate())
   url: string;
}
