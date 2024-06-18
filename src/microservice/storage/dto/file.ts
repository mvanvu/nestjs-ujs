import { IOmitType, Property } from '@lib';
import { $Enums } from '.prisma/storage';
import { FileEntity } from '../entity';

export class UploadDto {
   @Property({
      transform: { fromType: 'string', toType: 'toBoolean' },
      swagger: { description: 'Is this file is public?' },
   })
   isPublic: boolean;

   @Property({
      validate: { is: 'inArray', meta: Object.values($Enums.FileType) },
      swagger: { description: 'The type of file', type: $Enums.FileType, enum: $Enums.FileType },
   })
   fileType: $Enums.FileType;

   @Property({ swagger: { description: 'The binary file', type: 'file' } })
   file: Express.Multer.File;
}

export class FinalUploadDto extends IOmitType(FileEntity, ['id', 'url', 'bind']) {}
