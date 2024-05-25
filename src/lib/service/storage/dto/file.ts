import { IOmitType, EntityProperty } from '@lib/common';
import { FileType } from '.prisma/storage';
import { FileEntity } from '../entity';
import { ApiProperty } from '@nestjs/swagger';

const enumType = ['Image', 'Document', 'Unknown'];

export class UploadDto {
   @ApiProperty({ description: 'Is this file is public?', type: 'boolean' })
   @EntityProperty({ transform: { fromType: 'string', toType: 'toBoolean' } })
   isPublic: boolean;

   @ApiProperty({ description: 'The type of file', type: 'enum', enum: enumType })
   @EntityProperty({ validate: { is: 'inArray', meta: enumType } })
   fileType: FileType;

   @ApiProperty({ description: 'The binary file', type: 'file' })
   @EntityProperty()
   file: Express.Multer.File;
}

export class FinalUploadDto extends IOmitType(FileEntity, ['id', 'url', 'bind']) {}
