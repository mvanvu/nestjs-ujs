import { StreamableFile } from '@nestjs/common';
import { FileEntity } from '../entity';
import { UploadDto, FinalUploadDto } from '../dto';

export interface FileProviderInterface {
   upload(dto: UploadDto): Promise<FinalUploadDto | null>;
   getUrl(file: FileEntity): string;
   stream(file: FileEntity): Promise<StreamableFile>;
}
