import { BooleanSchema, EnumSchema, IOmitType, PropertySchema } from '@shared-library';
import { $Enums } from '.prisma/storage';
import { FileEntity } from '../entity';

export class UploadDto {
   @BooleanSchema({ fromString: true })
   isPublic: boolean;

   @EnumSchema(Object.values($Enums.FileType))
   fileType: $Enums.FileType;

   @PropertySchema({ swagger: { type: 'file' } })
   file: Express.Multer.File;
}

export class FinalUploadDto extends IOmitType(FileEntity, ['id', 'url', 'bind']) {}
