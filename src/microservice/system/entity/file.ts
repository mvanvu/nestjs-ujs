import { FileType, StorageProvider } from '.prisma/system';
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

   @(Schema.enum(Object.values(FileType)).decorate())
   type: FileType;

   @(Schema.content().decorate())
   mime: string;

   @(Schema.enum(Object.values(StorageProvider)).decorate())
   provider: StorageProvider;

   @(Schema.content().decorate())
   providerId: string;

   @(Schema.imageUri().decorate())
   url: string;
}
