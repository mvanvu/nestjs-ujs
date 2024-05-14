import { IOmitType, Property } from '@lib';
import { FileType } from '.prisma/storage';
import { FileEntity } from '../entity';

const enumType = ['Image', 'Document', 'Unknown'];

export class UploadDto {
   @Property({
      swagger: { description: 'Is this file is public?', type: 'boolean' },
      transform: { fromType: 'string', toType: 'toBoolean' },
   })
   isPublic: boolean;

   @Property({
      swagger: { description: 'The type of file', type: 'enum', enum: enumType },
      validate: { is: 'inArray', meta: enumType },
   })
   fileType: FileType;

   @Property({
      swagger: { description: 'The binary file', type: 'file' },
   })
   file: Express.Multer.File;
}

export class FinalUploadDto extends IOmitType(FileEntity, ['id', 'url', 'bind']) {}
