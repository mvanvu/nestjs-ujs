import { EnumSchema, IOmitType, PropertySchema, StringSchema } from '@shared-library';
import { $Enums } from '.prisma/storage';
import { FileEntity } from '../entity';

export class UploadDto {
   @StringSchema({ format: 'boolean', transform: 'format' })
   isPublic: boolean;

   @EnumSchema(Object.values($Enums.FileType))
   fileType: $Enums.FileType;

   @PropertySchema({ swagger: { type: 'file' } })
   file: Express.Multer.File;
}

export class FinalUploadDto extends IOmitType(FileEntity, ['id', 'url']) {}
