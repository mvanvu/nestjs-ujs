import { $Enums } from '.prisma/system';
import { FileEntity } from '../entity';
import { ClassRefSchema, Schema } from '@mvanvu/ujs';

export class UploadDto {
   @(Schema.strBool().decorate())
   isPublic: boolean;

   @(Schema.enum($Enums.FileType).decorate())
   fileType: $Enums.FileType;

   // File validation has already exists from the @UseInterceptors(FileInterceptor('file')) so we disable the schema validate here
   @(Schema.string().format('binary').validate(false).decorate())
   file: Express.Multer.File;
}

export class FinalUploadDto extends ClassRefSchema.Omit(FileEntity, ['id', 'url']) {}
