import { IOmitType } from '@shared-library';
import { $Enums } from '.prisma/storage';
import { FileEntity } from '../entity';
import { ApiProperty } from '@nestjs/swagger';
import { Schema } from '@mvanvu/ujs';

export class UploadDto {
   @Schema.strBool().decorate()
   isPublic: boolean;

   @Schema.enum(Object.values($Enums.FileType)).decorate()
   fileType: $Enums.FileType;

   @ApiProperty({ type: 'file' })
   @Schema.object().decorate()
   file: Express.Multer.File;
}

export class FinalUploadDto extends IOmitType(FileEntity, ['id', 'url']) {}
