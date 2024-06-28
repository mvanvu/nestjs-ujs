import { IOmitType, IsIn, Property, StringToType } from '@shared-library';
import { $Enums } from '.prisma/storage';
import { FileEntity } from '../entity';

export class UploadDto {
   @StringToType('boolean', { swagger: { description: 'Is this file is public?' } })
   isPublic: boolean;

   @IsIn(Object.values($Enums.FileType), { swagger: { description: 'The type of file' } })
   fileType: $Enums.FileType;

   @Property({ swagger: { description: 'The binary file', type: 'file' } })
   file: Express.Multer.File;
}

export class FinalUploadDto extends IOmitType(FileEntity, ['id', 'url', 'bind']) {}
