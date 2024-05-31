import { IOmitType, EntityProperty } from '@lib/common';
import { $Enums } from '.prisma/storage';
import { FileEntity } from '../entity';

export class UploadDto {
   @EntityProperty({
      transform: { fromType: 'string', toType: 'toBoolean' },
      swagger: { description: 'Is this file is public?' },
   })
   isPublic: boolean;

   @EntityProperty({
      validate: { is: 'inArray', meta: Object.values($Enums.FileType) },
      swagger: { description: 'The type of file', type: $Enums.FileType, enum: $Enums.FileType },
   })
   fileType: $Enums.FileType;

   @EntityProperty({ swagger: { description: 'The binary file', type: 'file' } })
   file: Express.Multer.File;
}

export class FinalUploadDto extends IOmitType(FileEntity, ['id', 'url', 'bind']) {}
