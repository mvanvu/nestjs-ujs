import { FileEntity, FinalUploadDto, UploadDto } from '@lib';
import { StreamableFile } from '@nestjs/common';

export interface FileProviderInterface {
   upload(dto: UploadDto): Promise<FinalUploadDto | null>;
   getUrl(file: FileEntity): string;
   stream(file: FileEntity): Promise<StreamableFile>;
}
